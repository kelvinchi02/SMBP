# Chat UI component 
# (Frontend handles the button, window, and message display)
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
        div(class = "chat-message message-assistant", div(class = "message-label", "Assistant"), div(class = "message-bubble", "Hello! The AI Assistant is currently undergoing maintenance for a system upgrade. We will be back online soon!"))
      ),
      div(class = "chat-loading", id = "chatLoading", "Thinking..."),
      div(
        class = "chat-input-area",
        div(
          class = "chat-input-group",
          tags$input(type = "text", class = "chat-input", id = "chatInput", placeholder = "Chat is currently offline...", onkeypress = "if(event.keyCode==13) sendMessage()"),
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

# ---------------------------------------------------------
# Placeholder for Future Hugging Face Integration
# ---------------------------------------------------------

# We keep the function name 'call_chatgpt' for now so app.R doesn't crash,
# but it now just returns a static message. 
# You can rename this to 'call_ai_model' later when you update app.R.

call_chatgpt <- function(message, api_key = NULL, max_retries = 3) {
  
  # Future: Add httr::POST request to Hugging Face Inference API here
  
  # Temporary simulated delay to make it feel "real"
  Sys.sleep(1)
  
  return("The AI system is currently being migrated to a new Hugging Face model. Please check back later for updates.")
}