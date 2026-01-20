$(document).on('shiny:inputchanged', function (event) {

    if(event.name == "goButton"){
        $("#downloadModel").css("display","")      
    }
    //ROCplot
    if(event.name == "printRocBtn"){
        printElement("ROCplot","");      
    }
    if(event.name == "printDistplot2Btn"){
        printElement("Distplot2","");      
    }
    if(event.name == "printDistplot3Btn"){
        printElement("Distplot3","");      
    }
    if(event.name == "printDistplotCBtn"){
        printElement("DistplotC","");      
    }

    if(event.name == "printSctplot2Btn"){
        printElement("Sctplot2","");      
    }
    if(event.name == "printSctplot3Btn"){
        printElement("Sctplot3","");      
    }
    if(event.name == "printSctplotCBtn"){
        printElement("SctplotC","");      
    }

    if (event.name === 'slctFunctionName') {
        var elems = document.querySelectorAll("div[id='subElementsFunction']");
        $.each(elems,function(key,value){
            // value.innerHTML = "";
            // value.remove();
        });
        var elemsMethod = document.querySelectorAll("div[id='subElements']");
        $.each(elemsMethod,function(key,value){
            // value.innerHTML = "";
            // value.remove();
        });
    
    }else if(event.name === "method"){
        var elems = document.querySelectorAll("div[id='subElements']");
        $.each(elems,function(key,value){
            // value.innerHTML = "";
            // value.remove();
        });
    }
    
});

$(document).on('shiny:connected', function (event) {
    $(".aboutSidebarPanel").attr("class","aboutSidebarPanel")
    $('#collapseExample').on('click', '.panel-heading', function() {
        var panelTitle = $(this).text().trim();
        console.log('Panel tıklandı: ' + panelTitle);
        
        // Shiny'ye bildirim gönder
        Shiny.onInputChange('panelClicked', {
          panelName: panelTitle,
          timestamp: Date.now()
        });
      });
    // document.querySelectorAll("ul[role='tablist']")[0].style.marginBottom  = "10px";
    //$("#RawData").css("width","");
    //$("#RawData").add("width","");
    // $(".col-sm-8").attr("class","col-sm-12")
    //document.getElementById("RawData").classList.add("col-sm-8")
    //document.getElementById("outputModelFit").classList.add("col-sm-8")
    //document.getElementById("outputPredictMainPanel").className = "col-sm-8";


});

function printElement(id,title){
    var divContents = document.getElementById(id).innerHTML;
    var a = window.open('', '');
    a.document.write('<html>');
    a.document.write('<body style="display: tabble-cell; vertical-align:middle">');
    a.document.write('<body > <h1>'+title+' <br>');
    a.document.write('<div style="margin-left:100px">')
    a.document.write(divContents);
    a.document.write('</div></body></html>');
    a.document.close();
    a.print();
}
