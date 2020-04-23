var pltDim = [0, 0];
$(document).on("shiny:filedownload", function(e){
  pltDim[0] = document.getElementById("pltmap").clientWidth;
  pltDim[1] = document.getElementById("pltmap").clientHeight;
  Shiny.onInputChange("pltDim", pltDim);
});
