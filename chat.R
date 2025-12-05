# Ensure httr2 and jsonlite are available
library(httr2)
library(jsonlite)

# -------------------------------------------------------------------------
# LLM Integration: Connecting to Groq API (Chat Function)
# -------------------------------------------------------------------------

# The call_chatgpt function now accepts the full conversation history list
call_chatgpt <- function(messages_list, api_key = NULL, max_retries = 3) {
  
  # --- Configuration ---
  GROQ_ENDPOINT <- "https://api.groq.com/openai/v1/chat/completions"
  GROQ_API_KEY <- Sys.getenv("GROQ_API_KEY")
  
  # Use the model ID from your template
  MODEL_NAME <- "llama-3.1-8b-instant" 
  
  if (is.null(GROQ_API_KEY) || GROQ_API_KEY == "") {
    return("Error: GROQ_API_KEY environment variable not set. Cannot connect to Groq.")
  }

  # --- Request Body Construction (Payload) ---
  # Aligned with your Groq template, but forcing stream=FALSE for R compatibility
  body <- list(
    model = MODEL_NAME,
    messages = messages_list,
    max_tokens = 1024,       # Increased to match template
    temperature = 1.0,       # Matched template
    top_p = 1.0,             # Matched template
    stream = FALSE           # CRITICAL: Keep FALSE for simple JSON parsing in R
  )
  
  # Debugging: Print the exact JSON being sent (optional, can be removed in prod)
  # message("--- Sending Request to Groq ---")
  # print(toJSON(body, auto_unbox=TRUE))

  # --- API Call Execution with Exponential Backoff ---
  for (attempt in 1:max_retries) {
    tryCatch({
      req <- request(GROQ_ENDPOINT) |>
        req_headers(
          "Authorization" = paste("Bearer", GROQ_API_KEY),
          "Content-Type" = "application/json"
        ) |>
        req_body_json(body) |>
        req_timeout(30)
        
      resp <- req_perform(req)
      
      # Check HTTP status code
      if (resp_status(resp) != 200) {
        stop(paste("HTTP Error:", resp_status(resp), resp_body_string(resp)))
      }
      
      # Parse response JSON
      result <- resp_body_json(resp, simplifyVector = TRUE)
      
      # Extract the generated text 
      # Standard Groq/OpenAI structure: choices -> [1] -> message -> content
      if (length(result$choices) > 0) {
        # Handle potential differences in list vs dataframe structure from jsonlite
        content <- if(is.data.frame(result$choices)) {
          result$choices$message$content[1]
        } else {
          result$choices[[1]]$message$content
        }
        
        if (!is.null(content) && content != "") {
          return(trimws(content))
        }
      } 
      
      stop("Received empty or invalid response content from Groq.")
      
    }, error = function(e) {
      message(paste("[AI ERROR] Attempt", attempt, "failed:", e$message))
      if (attempt < max_retries) {
        Sys.sleep(2^attempt) # Exponential backoff
      } else {
        return("I am currently experiencing connection issues and cannot process your request. Please try again later.")
      }
    })
  }
  
  return("I was unable to connect to the AI model after multiple retries.")
}

# -------------------------------------------------------------------------
# UI component (retains previous structure)
# -------------------------------------------------------------------------
chat_ui <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .chat-button {
          position: fixed; bottom: 2rem; right: 2rem; width: 60px; height: 60px;
          border-radius: 50%; background-color: #2c3e50; color: white; border: none;
          cursor: pointer; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
          display: flex; align-items: center; justify-content: center;
          font-size: 24px; transition: all 0.3s; z-index: 1000;
        }
        .chat-button:hover { background-color: #34495e; transform: scale(1.1); }
        .chat-panel {
          position: fixed; bottom: 5rem; right: 2rem; width: 380px; height: 550px;
          background-color: #ffffff; border: 1px solid #e0e0e0; border-radius: 8px;
          box-shadow: 0 8px 24px rgba(0, 0, 0, 0.15); display: none; flex-direction: column; z-index: 1000;
        }
        .chat-panel.active { display: flex; }
        .chat-header {
          background-color: #2c3e50; color: white; padding: 1rem 1.2rem;
          border-radius: 8px 8px 0 0; display: flex; align-items: center; justify-content: space-between;
        }
        .chat-header h4 { margin: 0; font-size: 1rem; font-weight: 600; }
        .chat-close {
          background: transparent; border: none; color: white; font-size: 1.2rem;
          cursor: pointer; padding: 0; width: 24px; height: 24px;
          display: flex; align-items: center; justify-content: center;
        }
        .chat-messages {
          flex: 1; overflow-y: auto; padding: 1rem; background-color: #fafbfc;
        }
        .chat-message { margin-bottom: 1rem; display: flex; flex-direction: column; }
        .message-user { align-items: flex-end; }
        .message-assistant { align-items: flex-start; }
        .message-bubble {
          max-width: 80%; padding: 0.75rem 1rem; border-radius: 12px;
          font-size: 0.9rem; line-height: 1.4; word-wrap: break-word;
        }
        .message-user .message-bubble { background-color: #2c3e50; color: white; }
        .message-assistant .message-bubble { background-color: #ffffff; color: #2c3e50; border: 1px solid #e0e0e0; }
        .message-label { font-size: 0.7rem; color: #6c757d; margin-bottom: 0.3rem; padding: 0 0.5rem; }
        .chat-input-area { padding: 1rem; border-top: 1px solid #e0e0e0; background-color: #ffffff; }
        .chat-input-group { display: flex; gap: 0.5rem; }
        .chat-input {
          flex: 1; padding: 0.75rem; border: 1px solid #dee2e6; border-radius: 6px;
          font-size: 0.9rem; outline: none;
        }
        .chat-input:focus { border-color: #2c3e50; }
        .chat-send {
          padding: 0.75rem 1.2rem; background-color: #2c3e50; color: white;
          border: none; border-radius: 6px; cursor: pointer; font-size: 0.9rem;
          font-weight: 600; transition: all 0.2s;
        }
        .chat-send:hover { background-color: #34495e; }
        .chat-send:disabled { background-color: #6c757d; cursor: not-allowed; }
        .chat-loading { display: none; padding: 0.75rem 1rem; font-size: 0.9rem; color: #6c757d; font-style: italic; }
        .chat-loading.active { display: block; }
      "))
    ),
    tags$button(class = "chat-button", onclick = "toggleChat()", icon("comment")),
    div(
      class = "chat-panel", id = "chatPanel",
      div(class = "chat-header", h4("Bus System Assistant"), tags$button(class = "chat-close", onclick = "toggleChat()", "×")),
      div(
        class = "chat-messages", id = "chatMessages",
        div(class = "chat-message message-assistant", div(class = "message-label", "Assistant"), div(class = "message-bubble", "Hello! I am your Smart Transit AI Assistant. How can I help you analyze bus operations?"))
      ),
      div(class = "chat-loading", id = "chatLoading", "Thinking..."),
      div(
        class = "chat-input-area",
        div(
          class = "chat-input-group",
          tags$input(type = "text", class = "chat-input", id = "chatInput", placeholder = "Ask about routes, delays, or crowding...", onkeypress = "if(event.keyCode==13) sendMessage()"),
          tags$button(class = "chat-send", id = "chatSend", onclick = "sendMessage()", "Send")
        )
      )
    ),
    tags$script(HTML("
      function toggleChat() { document.getElementById('chatPanel').classList.toggle('active'); }
      function sendMessage() {
        const input = document.getElementById('chatInput');
        const message = input.value.trim();
        if (!message) return;
        addMessage('user', message);
        input.value = '';
        document.getElementById('chatLoading').classList.add('active');
        document.getElementById('chatSend').disabled = true;
        Shiny.setInputValue('chat_message', { text: message, timestamp: Date.now() }, {priority: 'event'});
      }
      function addMessage(role, text) {
        const messagesDiv = document.getElementById('chatMessages');
        const messageDiv = document.createElement('div');
        messageDiv.className = `chat-message message-${role}`;
        const label = document.createElement('div');
        label.className = 'message-label';
        label.textContent = role === 'user' ? 'You' : 'Assistant';
        const bubble = document.createElement('div');
        bubble.className = 'message-bubble';
        bubble.textContent = text;
        messageDiv.appendChild(label);
        messageDiv.appendChild(bubble);
        messagesDiv.appendChild(messageDiv);
        messagesDiv.scrollTop = messagesDiv.scrollHeight;
      }
      Shiny.addCustomMessageHandler('chat_response', function(message) {
        document.getElementById('chatLoading').classList.remove('active');
        document.getElementById('chatSend').disabled = false;
        addMessage('assistant', message);
      });
    "))
  )
}