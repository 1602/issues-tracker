// pull in desired CSS/SASS files
// require( './styles/main.scss' );

let appData;

try {
    appData = JSON.parse(localStorage.appData);
    if (appData.user) {
        appData.accessToken = appData.user.secretKey;
        delete appData.user;
    }
    if (!appData.pinnedMilestones) {
        appData.pinnedMilestones = [];
    }
    if (!appData.columns) {
        appData.columns = [ "Icebox", "Backlog", "Current", "Done" ];
    }
    if (!appData.defaultRepositoryType) {
        appData.defaultRepositoryType = 'specified';
    }
    if (!appData.defaultRepository) {
        appData.defaultRepository = 'universalbasket/engineering';
    }
    if (!appData.recentRepos) {
        appDate.recentRepos = [ 'universalbasket/engineering' ];
    }
} catch(e) {
    appData =
        { accessToken: null
        , pinnedMilestones: []
        , columns: [ "Icebox", "Backlog", "Current", "Done" ]
        , defaultRepositoryType : 'specified'
        , defaultRepository : 'universalbasket/engineering'
        , recentRepos : [ 'universalbasket/engineering' ]
        };
}

// inject bundled Elm app into div#main
const Elm = require('../Main');
const elm = Elm.Main.embed( document.getElementById( 'main' ), appData );

window.onSignIn = function(googleUser) {
    elm.ports.googleAuth.send(googleUser.getAuthResponse()['id_token']);
};

elm.ports.saveData.subscribe(data => {
    localStorage.appData = JSON.stringify(data);
});

elm.ports.clipboard.subscribe(str => {
    copyTextToClipboard(str);
});

// copy-n-paste from:
// http://stackoverflow.com/questions/400212/how-do-i-copy-to-the-clipboard-in-javascript
function copyTextToClipboard(text) {
  var textArea = document.createElement("textarea");

  //
  // *** This styling is an extra step which is likely not required. ***
  //
  // Why is it here? To ensure:
  // 1. the element is able to have focus and selection.
  // 2. if element was to flash render it has minimal visual impact.
  // 3. less flakyness with selection and copying which **might** occur if
  //    the textarea element is not visible.
  //
  // The likelihood is the element won't even render, not even a flash,
  // so some of these are just precautions. However in IE the element
  // is visible whilst the popup box asking the user for permission for
  // the web page to copy to the clipboard.
  //

  // Place in top-left corner of screen regardless of scroll position.
  textArea.style.position = 'fixed';
  textArea.style.top = 0;
  textArea.style.left = 0;

  // Ensure it has a small width and height. Setting to 1px / 1em
  // doesn't work as this gives a negative w/h on some browsers.
  textArea.style.width = '2em';
  textArea.style.height = '2em';

  // We don't need padding, reducing the size if it does flash render.
  textArea.style.padding = 0;

  // Clean up any borders.
  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';

  // Avoid flash of white box if rendered for any reason.
  textArea.style.background = 'transparent';


  textArea.value = text;

  document.body.appendChild(textArea);

  textArea.select();

  try {
    var successful = document.execCommand('copy');
    var msg = successful ? 'successful' : 'unsuccessful';
    console.log('Copying text command was ' + msg);
  } catch (err) {
    console.log('Oops, unable to copy');
  }

  document.body.removeChild(textArea);
}
