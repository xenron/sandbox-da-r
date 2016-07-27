<script type="text/javascript">
  
function buttonClick(){
  
  // get and set up the drawing canvas
  
  var c=document.getElementById("myCanvas");
  var ctx=c.getContext("2d");
  ctx.font="30px Arial";
  
  // get the text from the UI
    
  var text = document.getElementById("textArea").value;
  
  // set up positional variables
  
  var textX = 150;
  var textY = 1;
  
  // define move function
    
  function move(){
      
    ctx.clearRect(0, 0, c.width, c.height);
    ctx.fillText(text, textX, textY * 5);
      
    if(textY++ < 40){
        
      setTimeout(move, 25); // delay between frames
    }
  }
    
  move(); // call function
}

</script> 