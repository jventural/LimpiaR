# LimpiaR

Limpieza de datos de encuestas en pocos pasos, para tesistas que empiezan en R.
Una función por paso, de una sola línea, que muestra en la consola qué hizo.
La comparten las librerías del curso **QuasiEx** (cuasiexperimental), **PredSEM**
(predictivo con SEM) y **RedPsy** (redes): al cargar cualquiera de ellas ya
tienes estas funciones.

## Instalación

```r
install.packages("remotes")
remotes::install_github("jventural/LimpiaR")
```

Si usas QuasiEx, PredSEM o RedPsy no hace falta: LimpiaR se instala con ellas.

## La limpieza completa

```r
library(LimpiaR)   # o library(QuasiEx), library(PredSEM), library(RedPsy)

base <- leer("Base_sin_limpiar.xlsx")                      # 1. el original no se toca
revisar(base, 1:10)                                        #    cómo llegó cada columna
base <- renombrar(base, edad = 4, sexo = 5, SCS = 9:34)    # 2. nombres cortos
base <- numero(base, c("edad", "SCS"))                     # 3. "19 años" -> 19
base <- recodificar(base, "BP", "Estoy en desacuerdo" = 1,
  "Ni de acuerdo ni en desacuerdo" = 2, "Estoy de acuerdo" = 3)
base <- unificar(base, "sexo", Femenino = c("fem", "mujer"), Masculino = "mas")  # 4.
base <- validar(base, "SCS", min = 1, max = 5)
base <- calidad(base, c("SCS", "BP"), id = "codigo")       # 6. marca, no borra
base <- invertir(base, "SCS1", min = 1, max = 5)
base <- puntaje(base, "SCS")
final <- excluir(base,                                      # 5. criterios en orden
  "No aceptó participar" = consentimiento == "No",
  "Menor de edad"        = edad < 18,
  "Envío repetido"       = repetido)
flujo(final)                                               # 7. figura del flujo
guardar(final, "Base_limpia.xlsx")                         #    datos + flujo + diccionario
```

## Funciones

| Paso | Función | Qué hace |
|---|---|---|
| 1 | `leer()` · `revisar()` | Abre Excel o CSV sin tocar el original y muestra cómo llegó cada columna |
| 2 | `renombrar()` | Nombres cortos; toma el número del enunciado y guarda el enunciado en un diccionario |
| 3 | `numero()` · `recodificar()` | Texto a número, según el instrumento |
| 4 | `unificar()` · `validar()` | Una sola forma por categoría; deja vacío lo imposible y dice dónde estaba |
| 5 | `excluir()` | Criterios del protocolo en orden; cada persona sale por el primero que cumple |
| 6 | `calidad()` · `invertir()` · `puntaje()` | Marca vacíos, respuestas planas y repetidos; invierte y suma |
| 7 | `flujo()` · `guardar()` | Figura del flujo de participantes y base limpia |

Las bases de ejemplo (`inst/extdata`) son **simuladas**: no corresponden a ninguna persona.

## Autor

José Ventura-León
