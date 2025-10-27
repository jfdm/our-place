let div = document.getElementById("quiztarget");
div.onload = function () {
    div.style.height =
        div.contentWindow.document.body.scrollHeight + 'px';
        }
