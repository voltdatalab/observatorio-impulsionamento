window.onload = function() {
    parent.postMessage(document.body.scrollHeight, '*');
};

window.onresize = function() {
    parent.postMessage(document.body.scrollHeight, '*');
};