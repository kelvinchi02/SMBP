library(shiny)

# -------------------------------------------------------------------------
# UI COMPONENT
# -------------------------------------------------------------------------
login_ui <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap');

        body { font-family: 'Poppins', sans-serif; margin: 0; padding: 0; }
        .login-wrapper { min-height: 100vh; display: flex; align-items: center; justify-content: center; background: #f8f9fa; padding: 20px; }
        .login-container { background: white; border-radius: 20px; padding: 50px; width: 100%; max-width: 450px; box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3); }
        .login-header { text-align: center; margin-bottom: 40px; }
        .login-header h1 { font-size: 2rem; font-weight: 700; color: #2c3e50; margin: 0 0 10px 0; letter-spacing: -0.02em; }
        .login-header p { font-size: 0.95rem; color: #6c757d; margin: 0; }
        
        .login-form { margin-top: 30px; }
        .form-group { margin-bottom: 25px; }
        .form-group label { display: block; font-size: 0.9rem; font-weight: 500; color: #495057; margin-bottom: 8px; }
        .form-group input { width: 100%; padding: 14px 16px; font-size: 0.95rem; border: 2px solid #e9ecef; border-radius: 10px; transition: all 0.3s; font-family: 'Poppins', sans-serif; }
        .form-group input:focus { outline: none; border-color: #2c3e50; box-shadow: 0 0 0 3px rgba(44, 62, 80, 0.1); }
        
        .login-btn { width: 100%; padding: 14px; font-size: 1rem; font-weight: 600; color: white; background: #2c3e50; border: none; border-radius: 10px; cursor: pointer; transition: all 0.3s; margin-top: 10px; font-family: 'Poppins', sans-serif; }
        .login-btn:hover { background: #34495e; transform: translateY(-2px); box-shadow: 0 10px 25px rgba(44, 62, 80, 0.3); }
        .login-btn:active { transform: translateY(0); }
        
        .login-footer { text-align: center; margin-top: 30px; padding-top: 20px; border-top: 1px solid #e9ecef; }
        .login-footer p { font-size: 0.85rem; color: #6c757d; margin: 0; }
        
        .error-message { background-color: #fff5f5; border: 1px solid #fc8181; color: #c53030; padding: 12px 16px; border-radius: 8px; margin-bottom: 20px; font-size: 0.9rem; display: none; }
        .error-message.show { display: block; animation: shake 0.5s; }
        @keyframes shake { 0%, 100% { transform: translateX(0); } 10%, 30%, 50%, 70%, 90% { transform: translateX(-5px); } 20%, 40%, 60%, 80% { transform: translateX(5px); } }
      "))
    ),

    div(
      class = "login-wrapper",
      div(
        class = "login-container",
        div(class = "login-header", h1("Smart Bus Management"), p("Please login to continue")),
        
        # Error Message Container
        div(id = "error-msg", class = "error-message"),

        div(
          class = "login-form",
          div(class = "form-group", tags$label("Username"), textInput("login_username", NULL, placeholder = "Enter your username", width = "100%")),
          div(class = "form-group", tags$label("Password"), passwordInput("login_password", NULL, placeholder = "Enter your password", width = "100%")),
          actionButton("login_btn", "Login", class = "login-btn")
        ),

        div(class = "login-footer", p("© 2025 Smart Bus Management Platform"))
      )
    ),

    # JavaScript to handle Enter key and Error visuals
    tags$script(HTML("
      $(document).on('shiny:value', function(event) {
        if (event.name === 'login_error_trigger') {
          var msg = event.value;
          if (msg && msg.length > 0) {
            $('#error-msg').text(msg).addClass('show');
            setTimeout(function() { $('#error-msg').removeClass('show'); }, 3000);
          }
        }
      });

      $(document).on('keypress', function(e) {
        if (e.which == 13) { $('#login_btn').click(); }
      });
    "))
  )
}

# -------------------------------------------------------------------------
# LOGIC COMPONENT
# -------------------------------------------------------------------------

authenticate_user <- function(username, password) {
  # Get credentials from Environment Variables (set in .env or Posit Cloud settings)
  valid_user <- Sys.getenv("LOGIN_USER")
  valid_pass <- Sys.getenv("LOGIN_PASS")
  
  # Basic validation
  if (username == "" || password == "") {
    return(list(success = FALSE, message = "Please enter both username and password"))
  }
  
  # Check credentials
  if (tolower(username) == tolower(valid_user) && password == valid_pass) {
    return(list(success = TRUE, message = "Success"))
  } else {
    return(list(success = FALSE, message = "Invalid username or password"))
  }
}