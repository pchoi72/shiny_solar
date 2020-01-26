server = function(input, output, session) {

  output$chart1 = renderGvis({
    gvisAnnotationChart(ute_daily,
                        datevar = "Date",
                        numvar = "totalMW",
                        idvar = "Utility",
                        options = list(width = 800))
  })
  
  output$chart2 = renderGvis({
    gvisAnnotationChart(inst_daily,
                        datevar = "Date",
                        numvar = "totalMW",
                        idvar = "Installer",
                        options = list(width = 800))
  })
  
  output$chart3 = renderGvis({
    gvisAnnotationChart(sec_daily,
                        datevar = "Date",
                        numvar = "totalMW",
                        idvar = "Sector",
                        options = list(width = 800))
  })
  
  output$chart4 = renderGvis({
    gvisColumnChart(qtrly_util, options = list(title = "California Investor Owned Utilities (PG&E, SCE, SDG&E)", 
                                               height = 300, 
                                               hAxis = "{slantedText:'true', slantedTextAngle:'45'}"))
  })
  
  output$chart5 = renderGvis({
    gvisColumnChart(qtrly_inst, options = list(title = "Third-Party Installers (SunRun, Tesla, Vivint, SunPower)",
                                               height = 300, 
                                               hAxis = "{slantedText:'true', slantedTextAngle:'45'}"))
  })
  
  output$chart6 = renderGvis({
    gvisColumnChart(qtrly_sect, options = list(title = "Sector Comparison (Residential, Commercial, Educational, etc)",
                                               height = 300, 
                                               hAxis = "{slantedText:'true', slantedTextAngle:'45'}"))
  })

  output$chart7 = renderGvis({
    gvisComboChart(PGE_combo, xvar = "Yr.Qtr",
                   yvar = c("MW", "YoY"),
                   options = list(seriesType = "bars",
                                  title = "PGE Solar Applications by Quarter",
                                  colors = "['lightgrey', 'red']",
                                  height = 300,
                                  series = "{1: {type:'line', targetAxisIndex:'1'}}",
                                  hAxis = "{slantedText:'true', slantedTextAngle:'45'}",
                                  vAxes = "[{title:'Total MW',format:'#'},{title:'YoY % Change',format:'#%'}]"))
    })
  
  output$chart8 = renderGvis({
    gvisComboChart(SCE_combo, xvar = "Yr.Qtr",
                   yvar = c("MW", "YoY"),
                   options = list(seriesType = "bars",
                                  title = "SCE Solar Applications by Quarter",
                                  colors = "['lightgrey', 'red']",
                                  height = 300,
                                  series = "{1: {type:'line', targetAxisIndex:'1'}}",
                                  hAxis = "{slantedText:'true', slantedTextAngle:'45'}",
                                  vAxes = "[{title:'Total MW',format:'#'},{title:'YoY % Change',format:'#%'}]"))
  })
  
  output$chart9 = renderGvis({
    gvisComboChart(SDGE_combo, xvar = "Yr.Qtr",
                   yvar = c("MW", "YoY"),
                   options = list(seriesType = "bars",
                                  title = "SDGE Solar Applications by Quarter",
                                  colors = "['lightgrey', 'red']",
                                  height = 300,
                                  series = "{1: {type:'line', targetAxisIndex:'1'}}",
                                  hAxis = "{slantedText:'true', slantedTextAngle:'45'}",
                                  vAxes = "[{title:'Total MW',format:'#'},{title:'YoY % Change',format:'#%'}]"))
  })
  
  output$chart10 = renderGvis({
    gvisComboChart(TSLA_combo, xvar = "Yr.Qtr",
                   yvar = c("MW", "YoY"),
                   options = list(seriesType = "bars",
                                  title = "TSLA (SCTY) Solar Applications by Quarter",
                                  height = 300,
                                  series = "{1: {type:'line', targetAxisIndex:'1'}}",
                                  hAxis = "{slantedText:'true', slantedTextAngle:'45'}",
                                  vAxes = "[{title:'Total MW',format:'#'},{title:'YoY % Change',format:'#%'}]"))
  })
  
  output$chart11 = renderGvis({
    gvisComboChart(VSLR_combo, xvar = "Yr.Qtr",
                   yvar = c("MW", "YoY"),
                   options = list(seriesType = "bars",
                                  title = "VSLR Solar Applications by Quarter",
                                  height = 300,
                                  series = "{1: {type:'line', targetAxisIndex:'1'}}",
                                  hAxis = "{slantedText:'true', slantedTextAngle:'45'}",
                                  vAxes = "[{title:'Total MW',format:'#'},{title:'YoY % Change',format:'#%'}]"))
  })
  
  output$chart12 = renderGvis({
    gvisComboChart(RUN_combo, xvar = "Yr.Qtr",
                   yvar = c("MW", "YoY"),
                   options = list(seriesType = "bars",
                                  title = "RUN Solar Applications by Quarter",
                                  height = 300,
                                  series = "{1: {type:'line', targetAxisIndex:'1'}}",
                                  hAxis = "{slantedText:'true', slantedTextAngle:'45'}",
                                  vAxes = "[{title:'Total MW',format:'#'},{title:'YoY % Change',format:'#%'}]"))
  })
  
  output$chart13 = renderGvis({
    gvisComboChart(SPWR_combo, xvar = "Yr.Qtr",
                   yvar = c("MW", "YoY"),
                   options = list(seriesType = "bars",
                                  title = "SPWR Solar Applications by Quarter",
                                  height = 300,
                                  series = "{1: {type:'line', targetAxisIndex:'1'}}",
                                  hAxis = "{slantedText:'true', slantedTextAngle:'45'}",
                                  vAxes = "[{title:'Total MW',format:'#'},{title:'YoY % Change',format:'#%'}]"))
  })
  
  output$map = renderGvis({
    gvisGeoChart(ca_countiesMW2019, locationvar = "LatLong", colorvar = "MW", hovervar = "County",
                 options = list(region="US-CA", 
                                resolution = "provinces", 
                                displayMode = "markers",
                                width = 535,
                                height = 350,
                                keepAspectRation = "false",
                                sizeAxis = "{minValue: 0,  maxSize: 30}",
                                magnifyingGlass = "{enable: true, zoomFactor: 10}",
                                backgroundColor = "white",
                                colorAxis="{values:[0,140], colors:[\'white', \'green']}"))
  })
  
  output$pie = renderGvis({
    gvisPieChart(ca_pie_2019, labelvar = "County", numvar = "MW",
                 options = list(width = 535,
                                height = 350,
                                sliceVisibilityThreshold = 0.05,
                                pieHole=0.3,
                                pieSliceText="label"))
  })
  
  output$table = DT::renderDataTable(DT::datatable({
    if (input$util != "All") {
      solar = solar[solar$Utility == input$util,]
    }
    if (input$inst != "All") {
      solar = solar[solar$Installer == input$inst,]
    }
    if (input$sect != "All") {
      solar = solar[solar$Sector == input$sect,]
    }
    if (input$cty != "All") {
      solar = solar[solar$County == input$cty,]
    }
    solar
  }))
}