#' Create a standard bslib card with a plot output
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @importFrom bslib card
plot_card <- function(header, ...) {
  bslib::card(
    full_screen = TRUE,
    # min_height = "500px",
    bslib::card_header(header),
    bslib::card_body(...)
  )
}

# Function that builds the description text block from provided text
#'
#' @param lang One of ES (Espanol) or EN (English) 
#'
#' @returns
#'
#' @examples
#' \dontrun
build_desc <- function(lang){
  if(lang == 'ES'){
    p1 <- 'Coral Reef Alliance (CORAL) es una ONG dedicada a salvar los \
  arrecifes de coral del mundo a través de ciencia innovadora y el compromiso \
  con la comunidad. Mediante la colaboración con socios locales, CORAL trabaja \
  para reducir las amenazas directas a los arrecifes y promover soluciones \
  escalables y efectivas para su protección. El objetivo de CORAL es \
  establecer una red de arrecifes grandes, diversos, conectados y bien \
  gestionados. Para lograrlo, CORAL se enfoca en mitigar las amenazas locales \
  clave a través de sus estrategias de Agua Limpia y Pesquerías Sostenibles en \
  dos regiones de arrecifes de importancia crítica: Hawái y el Caribe \
  Occidental.'
    p2_1 <- 'Aquí presentamos los datos recopilados para la estrategia de \
  Pesquerías Sostenibles en la Bahía de Tela desde 2015 hasta la fecha. Estos \
  datos fueron recolectados utilizando el protocolo desarrollado por' 
    p2_2 <- tags$a('Rivera (2017)', href="https://coral.org/wp-content/uploads/2021/09/Plan-de-Manejo-v3.pdf")
    p2_3 <- 'el cual se enfoca en la recolección de cuatro tipos de datos:'
    items <- c(
      'Datos físicos y meteorológicos', 
      'Datos socioeconómicos', 
      'Datos de esfuerzo pesquero',
      'Datos biológicos, en colaboración con las comunidades a través de \
    Científicos Comunitarios.'
    )
    p3 <- 'La Bahía de Tela se encuentra ubicada en el Municipio de Tela, \
  Departamento de Atlántida, Honduras. La zona cuenta con tres áreas \
  protegidas marino-costeras: el Parque Nacional Blanca Janeth Kawas \
  (PNBJK; Decreto 154-94), el Parque Nacional Punta Izopo (PNPI; 261-2000) y \
  el Refugio de Vida Silvestre Bahía de Tela (RVSBT; Decreto 132-2017). Junto \
  con el Jardín Botánico Lancetilla (JBL; Decreto 48-90), estas áreas forman \
  el Subsistema de Áreas Protegidas Bahía de Tela. Estas zonas destacan por sus\
  diversos ecosistemas y su aporte a los medios de vida de las poblaciones \
  locales. La pesca se destaca como una de las principales actividades \
  económicas en el Municipio de Tela (Randazzo Eisemann, 2014), donde más de \
  1,111 pescadores artesanales dependen de los recursos marinos para su \
  subsistencia (Base de datos DIGEPESCA-Tela, 2018). En 2018 se estableció el \
  primer Plan de Manejo Pesquero para la Bahía de Tela (Rivera, 2018), liderado\ 
  por el Comité Interinstitucional de Ambiente y Áreas Protegidas del Municipio\
  de Tela (CIAT) y ejecutado de manera colaborativa con las comunidades \
  pesqueras de la Bahía de Tela. La base de datos muestra mejoras en la \
  sostenibilidad de los recursos marinos tras la implementación de estas\ 
  medidas.'
    p4 <- 'Este trabajo es posible gracias al Comité Interinstitucional de \
  Ambiente y Áreas Protegidas del Municipio de Tela (CIAT), la Dirección \
  General de Pesca y Acuicultura (DIGEPESCA), el Instituto de Conservación \
  Forestal (ICF), las ONGs PROLANSATE y AMATELA, los Científicos Comunitarios \
  y todos los pescadores de la Bahía de Tela.'
  } else {
    p1 <- 'Coral Reef Alliance (CORAL) is an NGO dedicated to saving the world’s coral reefs 
    through cutting-edge science and community engagement. By collaborating with local partners, 
    CORAL works to reduce direct threats to reefs and promote scalable, effective solutions 
    for their protection. CORAL’s goal is to establish a network of large, diverse, connected,
    and well-managed reefs. To achieve this, CORAL focuses on mitigating key local threats 
    through its Clean Water and Sustainable Fisheries strategies in two critically important
    reef regions: Hawaii and the Western Caribbean.'
    p2_1 <- 'Here, we present the data collected for the Sustainable Fisheries strategy in Tela Bay from 2015 to the present. 
    This data was gathered using the protocol developed by ' 
    p2_2 <- tags$a('Rivera (2017)', href="https://coral.org/wp-content/uploads/2021/09/Plan-de-Manejo-v3.pdf")
    p2_3 <- ', which emphasizes the collection of four types of data:'
    items <- c(
      'Physical and meteorological data', 
      'Socioeconomic data', 
      'Fishing effort data, ',
      'Biological data, in collaboration with communities through Community Scientists.'
    )
    p3 <- 'Tela Bay is located in the Municipality of Tela, Atlántida Department, Honduras. 
    The area encompasses three coastal marine protected areas: the Blanca Janeth Kawas 
    National Park (PNBJK; Decree 154-94), Punta Izopo National Park (PNPI; 261-2000), and 
    the Tela Bay Wildlife Refuge (RVSBT; Decree 132-2017). Along with the Lancetilla 
    Botanical Garden (JBL; Decree 48-90), these areas form the Tela Bay Protected Areas 
    Subsystem. These zones are notable for their diverse ecosystems and their contribution 
    to the livelihoods of local populations. Fishing is one of the main economic activities 
    in the Municipality of Tela (Randazzo Eisemann, 2014), where more than 1,111 artisanal 
    fishers depend on marine resources for their subsistence (DIGEPESCA-Tela Database, 2018). 
    In 2018, the first Fisheries Management Plan for Tela Bay was established (Rivera, 2018), 
    led by the Interinstitutional Committee on Environment and Protected Areas of the
    Municipality of Tela (CIAT) and implemented collaboratively with the fishing communities of 
    Tela Bay. The database shows improvements in the sustainability of marine resources 
    following the implementation of these measures.'
    p4 <- 'This work is made possible thanks to the Inter-Institutional Committee on 
    Environment and Protected Areas of the Municipality of Tela (CIAT), the General 
    Directorate of Fisheries and Aquaculture (DIGEPESCA), the Forest Conservation Institute 
    (ICF), local NGOs PROLANSATE and AMATELA, the Community Scientists, and all the fishers 
    of Tela Bay.'
  }
  # Creating description block
  tagList(
    h1(ifelse(lang == 'ES','Quiénes somos','Who we are')),
    p(p1),
    h1(ifelse(lang == 'ES','Contexto de esta plataforma','Context of this platform')),
    p(HTML(paste(p2_1, p2_2, p2_3))),
    tags$ol((list_to_li(items))),
    h1(ifelse(lang == 'ES','Descripción de la región','Description of the region')),
    p(p3),
    h1(ifelse(lang == 'ES','Agradecimientos','Acknowledgment')),
    p(p4),
    tags$br()
  )
}
