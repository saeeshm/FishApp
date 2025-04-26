$( document ).ready(function() {
  // document.getElementById('num_muestreos_title').innerText = 'Borp';
  function waitForElement(selector) {
    return new Promise(resolve => {
      const intervalId = setInterval(() => {
        const element = document.getElementById(selector);
        if (element) {
          clearInterval(intervalId);
          resolve(element);
        }
      }, 100);
    });
  }
  
  function defaultTextForElement(selector, defaultText) {
    waitForElement(selector).then((element) => {
      element.innerText = defaultText;
    });
  }

  defaultTextForElement('datGen-num_muestreos_title','Número de muestreos');
  defaultTextForElement('datGen-num_dias_muestreo_title','Dias muestreado');
  defaultTextForElement('datGen-num_comunidades_title','Comunidades muestreadas');
  defaultTextForElement('datGen-peso_muestreado_title','Peso total muestreado');
  defaultTextForElement('datGen-num_especies_title','Especies muestreadas');
  defaultTextForElement('datGen-num_individuos_title','Individuos muestreados');
  
});