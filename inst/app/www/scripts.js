$( document ).ready(function() {
 // Check if the class "upload_button" exists
var uploadButtonExists = document.querySelector('.upload_button');

// Create a <style> element
var styles = document.createElement('style');

// Define the CSS styles
var css = `
  div#allocation-ux-upload_ui {
      float: right;
      width: 50%;
      font-size: 12px;
      margin-top: 22px;
      height: 12px;
  }

  span.btn.btn-default.btn-file {
      font-size: 11px;
  }

  input.form-control {
      font-size: 12px;
  }

  .form-group.shiny-input-container.method {
      width: 50% !important;
      float: left;
  }
`;

// Add the CSS styles to the <style> element
styles.appendChild(document.createTextNode(css));

// Check if the class "upload_button" exists
if (uploadButtonExists) {
  // Append the <style> element to the <head> tag
  document.head.appendChild(styles);
} else {
  // Remove the <style> element from the <head> tag
  var existingStyles = document.querySelectorAll('style');
  existingStyles.forEach(function(style) {
    if (style.textContent === css) {
      style.parentNode.removeChild(style);
    }
  });
}

});
