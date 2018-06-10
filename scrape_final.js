var url ='http://www.oddsportal.com/matches/soccer/';
var page = new WebPage() 
var fs = require('fs'); 

page.settings.userAgent = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36';

page.open(url, function (status) { 
         just_wait(); 
}); 


function just_wait() { 
    setTimeout(function() { 
              fs.write('1.html', page.content, 'w'); 
           phantom.exit(); 
    }, 2500); 
} 
