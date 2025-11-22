# Unified styles for all dashboard pages

common_styles <- HTML("
  /* Page wrapper */
  .page-wrapper {
    background-color: #ffffff;
    min-height: 100vh;
  }
  
  /* Modern header */
  .page-header {
    background-color: #ffffff;
    border-bottom: 1px solid #e0e0e0;
    padding: 1.5rem 0;
    margin-bottom: 3rem;
  }
  
  .page-header .container {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 2rem;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  
  .page-header h2 {
    font-size: 1.5rem;
    font-weight: 600;
    color: #2c3e50;
    margin: 0;
    letter-spacing: -0.02em;
  }
  
  .page-header .btn-light {
    background-color: transparent;
    border: 1px solid #dee2e6;
    color: #495057;
    padding: 0.5rem 1.2rem;
    font-size: 0.9rem;
    transition: all 0.2s;
  }
  
  .page-header .btn-light:hover {
    border-color: #adb5bd;
    color: #2c3e50;
  }
  
  /* Main content container */
  .page-content {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 2rem 3rem 2rem;
  }
  
  /* Section styles */
  .page-section {
    margin-bottom: 4rem;
  }
  
  .section-title {
    font-size: 0.75rem;
    font-weight: 600;
    color: #6c757d;
    text-transform: uppercase;
    letter-spacing: 0.12em;
    margin-bottom: 2rem;
  }
  
  /* Footer */
  .page-footer {
    background-color: #ffffff;
    border-top: 1px solid #e0e0e0;
    padding: 2rem 0;
    margin-top: 3rem;
  }
  
  .page-footer .container {
    max-width: 1400px;
    margin: 0 auto;
    padding: 0 2rem;
    text-align: center;
  }
  
  .page-footer p {
    font-size: 0.85rem;
    color: #6c757d;
    margin: 0;
  }
  
  /* Input controls */
  .page-content .bootstrap-select .btn,
  .page-content .form-control {
    background-color: transparent;
    border: 1px solid #dee2e6;
    font-size: 0.9rem;
  }
  
  /* Cards - flat style */
  .page-content .card {
    background-color: transparent;
    border: none;
    box-shadow: none;
  }
  
  .page-content .card-header {
    background-color: transparent;
    border: none;
    padding: 0 0 1rem 0;
    font-size: 0.9rem;
    font-weight: 600;
    color: #495057;
  }
  
  .page-content .card-body {
    padding: 0;
  }
")