var url ='http://xin.baidu.com/detail/compinfo?pid=A6nozz*hfOIoVHRwHC5YaNbi28CMBGkPggoE';
var company_html = './html/BDXin.html';
var page = new WebPage();
var fs = require('fs');



page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write(company_html, page.content, 'w');
            phantom.exit();
    }, 25000);
}
