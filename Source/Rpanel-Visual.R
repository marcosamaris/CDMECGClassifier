
WinAdd("tt", title = "Proceso KDD de ECG's de Infarto Agudo de Miocardio", pos =NULL,height=600, width=800)
MenuAdd("$Tk.tt/&Cargar DB")
MenuAdd("$Tk.tt/&Preprocesamiento")
MenuAdd("$Tk.tt/&Transformación")
MenuAdd("$Tk.tt/C&lasificación")
MenuAdd("$Tk.tt/&Graficos")
MenuAdd("$Tk.tt/&Ayuda")

MenuNames()

(MenuItems("$Tk.tt/&Cargar DB"))  # Still nothing in it
MenuAddItem("$Tk.tt/&Cargar DB", "C&argar .txt ECG's", "CargarECGInfartados();CargarECGSanos()")
#MenuAddItem("$Tk.tt/&Cargar DB", "&Guardar .RData Ecg's", "GuardarRDatada()")
MenuAddItem("$Tk.tt/&Cargar DB", "&Salir", "WinDel('tt')")


(MenuItems("$Tk.tt/&Preprocesamiento"))  # Still nothing in it
MenuAddItem("$Tk.tt/&Preprocesamiento", "&Filtrar ECG", "FiltrarMuestras()")
MenuAddItem("$Tk.tt/&Preprocesamiento", "&Obtener HRV", "HRV()")

(MenuItems("$Tk.tt/&Transformación"))  # Still nothing in it
MenuAddItem("$Tk.tt/&Transformación", "&Normalizar", "Normalizar()")
MenuAddItem("$Tk.tt/&Transformación", "&SAX", "SAX()")
MenuAddItem("$Tk.tt/&Transformación", "M&atrix CDM", "CDMMatrix()")

(MenuItems("$Tk.tt/C&lasificación"))  # Still nothing in it
MenuAddItem("$Tk.tt/C&lasificación", "&Agnes CDM", "ClassAgnes()")
MenuAddItem("$Tk.tt/C&lasificación", "&Diana CDM", "ClassDiana()")

(MenuItems("$Tk.tt/&Graficos"))  # Still nothing in it
MenuAddItem("$Tk.tt/&Graficos", "&Intervalos RR", "VisualizeRR()")
MenuAddItem("$Tk.tt/&Graficos", "&HRV Electrocardiograma", "VisualizeHRV()")
MenuAddItem("$Tk.tt/&Graficos", "&Clasificación Agnes", "dev.off();plot(ag, ask = FALSE, which.plots = NULL,main='CDM de ECG Infartados Vs. Sanos', ylab='Altura', xlab='Medida CDM')")
MenuAddItem("$Tk.tt/&Graficos", "&Clasificación Diana", "dev.off();plot(di, ask = FALSE, which.plots = NULL,main='CDM de ECG Infartados Vs. Sanos', ylab='Altura', xlab='Medida CDM')")

(MenuItems("$Tk.tt/&Ayuda"))  # Still nothing in it
MenuAddItem("$Tk.tt/&Ayuda", "&Creditos", "cat('Este trabajo fue realizado como proyecto de investigación de Maestría en la Universidad Industrial de Santander\n 
Los autores son Marcos Amaris, Victor Martinez y Pablo Guillén.\n 
Para cualquier inquietud escribir al email marcos.amaris@gmail.com')")

#Creación de la ventana


elim <- function(){
  elim.panel = rp.control(title = "Seleccionar DF", size = c(100, 100), panelname = "Elim")
  rp.textentry(elim.panel, var=zzdf, title="ECG a Graficar: ",initval="")
  rp.button(elim.panel, title = "Ejecutar", action = ej, quitbutton = TRUE)
}
#Creamos la acción de la ventana
ej <- function(panel) {
  cat(panel$zzdf)
  panel
}
#elim()
