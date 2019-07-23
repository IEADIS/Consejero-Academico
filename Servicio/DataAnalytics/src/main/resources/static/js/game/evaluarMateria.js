var evaluarMateria = (function () {
    
    var updateResults = function (result) {
        result.nota3 = Math.round(Math.abs(result.nota3) * 100) / 100;
        result.rSquared = Math.round(result.rSquared * 10000) / 100;
        var tittle = "";
        var image = "";
        var content = "Según el proceso de análisis, la nota que sacarías en el ultimo tercio rondaria entre el valor de " +
                    result.nota3 + ", con una confiabilidad en la estimación del " + result.rSquared + "%";;
        var decision = "";
        if(result.decision === 'Cancele'){
            tittle = "JUMM.. ESTA COMPLICADO";
            image = "/images/mal.png";
            decision = "Lo mejor que puedes hacer es Cancelar";
        }else{
            tittle = "LO ESTAS HACIENDO BIEN";
            image = "/images/bien.png";
            decision = "No te descuides";
        }
        document.getElementById("status").src = image;
        document.getElementById("tercio3").innerHTML = tittle;
        document.getElementById("rSquared").innerHTML = content;
        document.getElementById("decision").innerHTML = decision;
    };
    
    var plot = function (id, src){
        $(id).html(
            '<iframe id="Plot1"'
            +        'style="text-align: center;"'
            +    'name="Pimb"'
            +    'title="Plot"'
            +    'width="800"'
            +    'height="500"'
            +    'frameborder="5"'
            +    'scrolling="no"'
            +    'src="/html/' + src + '">'
            +'</iframe>'
        );
    };
    
    var plotImg = function (id, src, tittle){
        $(id).html(
            '<img src="/html/img/' + src +  '" width="85%" alt="' + tittle +  '"/>'
        );
    };
    
    return {

        loadMaterias : function () {
            $.get("/evaluate/currentAsignatures",
                    function (data) {
                        console.log("LookingFor Asignatures");
                        console.log("Adding Asignatures");
                        console.log(data);
                        for (var i = 0, len = data.length; i < len; i++) {
                            $('#materias').append("<option value='" + data[i] + "'>");
                        }
                    }
            ).fail(
                    function (data) {
                        alert(data["responseText"]);
                    }

            );
        },
        
        makeEstimate : function () {
            console.log($("#materia").val());
            console.log($("#tercio1").val());
            console.log($("#tercio2").val());
            var Materia = {nombre: $("#materia").val(), nota1 : $("#tercio1").val(), nota2 : $("#tercio2").val()};
            
            jQuery.ajax({
                url: "/evaluate/estimate",
                type: "POST",
                data: JSON.stringify(Materia),
                dataType: "json",
                contentType: "application/json; charset=utf-8",
                success: function (result) {
                    updateResults(result);
                }
            });
        },
        addPlotBySubjects : function (){
            if ($("#materia").val() === "PIMB") {
                plot('#plot1', "3dPlotPimb.html");
                plot('#plot1', "plotPimb3dTercios.html");
            } else if ($("#materia").val() === "MBDA"){
                plot('#plot1', "3dplotMbda.html");
                plot('#plot2', "finalVspromMbda.html");
            }
        },
        addPlotByAnomalies : function (){
            if ($("#department").val() === "Sistemas") {
                plotImg('#plot1', "cancelVsSemsSistemas.png");
                plot('#plot2', "Anom_notasVsSemsSistemas.html");
            } else if ($("#department").val() === "Matematicas"){
                plotImg('#plot1', "cancelVsSemsMatematicas.png");
                plot('#plot2', "Anom_notasVsSemsMatematicas.html");
            } else if ($("#department").val() === "Humanidades"){
                plotImg('#plot1', "cancelVsSemsHumanidades.png");
                plot('#plot2', "Anom_notasVsSemsHumanidades.html");
            } else if ($("#department").val() === "Industrial"){
                plotImg('#plot1', "cancelVsSemsIndustrial.png");
                plot('#plot2', "Anom_notasVsSemsIndustrial.html");
            } else if ($("#department").val() === "Electronica"){
                plotImg('#plot1', "cancelVsSemsElectronica.png");
                plot('#plot2', "Anom_notasVsSemsElectronica.html");
            } else if ($("#department").val() === "Electrica"){
                plotImg('#plot1', "cancelVsSemsElectrica.png");
                plot('#plot2', "Anom_notasVsSemsElectrica.html");
            }
        }

    };

})();


