///PROYECTO FINAL
///METODOLOGIA DE LA PROGRAMACIÓN  -  BUAP FCC

//  1) Alta de alumnos 
SubProceso alta(a Por Referencia,nom Por Referencia,mat Por Referencia,calf Por Referencia,prom Por Referencia)
	//Definir  variables
	Definir op2,op3 Como Caracter
	Definir i,j,c Como Entero
	c <- 6
	Repetir
		a <- a + 1
		Repetir
			Limpiar Pantalla
			info
			//Ingreso de información
			Escribir "-- DADA DE ALTA NUEVO ALUMNO --"
			Escribir ''
			Escribir "  ||  ALUMNO #",a,"  ||"
			Escribir ''
			Escribir "Nombre:" Sin Saltar
			Leer nom[a]
			Escribir "Matricula:" Sin Saltar
			Leer mat[a]
			Escribir ''
			Escribir "CALIFICACIONES BIMESTRALES"
			para i <- 1 hasta c Con Paso 1 Hacer
				Repetir
					Escribir "    Bimestre ",i,": " Sin Saltar
					Leer calf[a,i]
					si calf[a,i] > 10 o calf[a,i] < 0 Entonces
						error
					FinSi
				Hasta Que calf[a,i] <= 10 y calf[a,i] >= 0
			FinPara
			//Función promedio y guardado en arreglo
			prom[a] <- promedio(calf,c,a)
			//Confirmación 1
			Escribir ''
			Escribir "¿Los datos son correctos? (S/N)"
			Leer op2
		Hasta Que op2 = "s" o op2 = "S"
		//Confirmación 2
		Escribir "¿Desea agregar otro alumno? (S/N)"
		Leer op3
	Hasta Que op3 = "n" o op3 = "N"
FinSubProceso

//  2) Baja de alumnos
SubProceso baja(nom Por Referencia,mat Por Referencia,calf Por Referencia,prom Por Referencia,a Por Referencia,id)
	//Definir  variables
	Definir op2 Como Caracter
	Definir i,j,c Como Entero
	c <- 6
	Limpiar Pantalla
	info
	Escribir "-- DADA DE BAJA DE ALUMNOS --"
	//Herramienta Busqueda
	hbus(id,a,nom,mat,calf,prom)
	Escribir '==========================='
	//Confirmación 1
	Escribir "¿Dar de baja? (S/N)"
	leer op2
	Si op2 = 's' o op2 = 'S' Entonces
		//Eliminar información del ultimo alumno
		si id = a Entonces
			a <- a - 1
		SiNo
			//Desplazar información del estudiante n al n-1
			para i <- id+1 hasta a Con Paso 1 Hacer
				nom[i-1] <- nom[i]
				mat[i-1] <- mat[i]
				para j <- 1 hasta c Con Paso 1 Hacer
					calf[i-1,j] <- calf[i,j]
				FinPara
				prom[i-1] <- prom[i]
				a <- a - 1
			FinPara
		FinSi
		Escribir ''
		Escribir "[!] ALUMNO DADO DE BAJA CORRECTAMENTE [!]"
	FinSi
FinSubProceso

//  3) Modificar datos de alumno (nombre)
SubProceso modNom(id,a,nom Por Referencia,mat,calf,prom)
	Limpiar Pantalla
	info
	Escribir " - MODIFICAR NOMBRE DE UN ALUMNO - "
	//Herramienta Busqueda
	hbus(id,a,nom,mat,calf,prom)
	Escribir ''
	Escribir "INGRESAR NUEVO NOMBRE:" Sin Saltar
	Leer nom[id]
FinSubProceso

//  4) Buscar un alumno (por nombre o matricula)
SubProceso busqueda(id,a,nom,mat,calf,prom)
	Limpiar Pantalla
	info
	Escribir "-- BUSQUEDA: INFORMACIÓN DE UN ALUMNO --"
	//Herramienta Busqueda
	hbus(id,a,nom,mat,calf,prom)
FinSubProceso

//   5) Alumno con mayor aprovechamiento
SubProceso mejor(nom,mat,calf,prom,a)
	//Definir  variables
	Definir j,i,recv,c Como Entero
	Definir recp Como Real
	recp <- 0
	c <- 6
	Limpiar Pantalla
	info
	Escribir " - ALUMNO CON MEJOR APROVECHAMIENTO - "
	Escribir ''
	//Indentificar al mejor promedio
	para j <- 1 hasta a Con Paso 1 Hacer
		si prom[j] > recp Entonces
			recp <- prom[j]
			recv <- j
		FinSi
	FinPara
	//Muestra de información
	Escribir "    ||  ALUMNO #",recv,"  ||"
	Escribir ''
	Escribir "Nombre: " Sin Saltar
	Escribir nom[recv]
	Escribir "Matricula: " Sin Saltar
	Escribir mat[recv]
	Escribir ''
	Escribir "CALIFICACIONES BIMESTRALES"
	para i <- 1 hasta c Con Paso 1 Hacer
		Escribir "     Bimestre ",i,": " Sin Saltar
		Escribir calf[recv,i]
	FinPara
	Escribir ''
	Escribir "Promedio anual: ",prom[recv]
FinSubProceso

//   6) Listado de todos los alumnos
SubProceso listTotal(a,nom,mat,calf,prom)
	//Definir  variables
	Definir i,j,c Como Entero
	c <- 6
	Limpiar Pantalla
	info
	Escribir "- LISTADO TOTAL DE ALUMNOS -"
	Escribir ''
	para j <- 1 hasta a Con Paso 1 Hacer
		//Muestra de información
		Escribir "    ||  ALUMNO #",j,"  ||"
		Escribir ''
		Escribir "Nombre: " Sin Saltar
		Escribir nom[j]
		Escribir "Matricula: " Sin Saltar
		Escribir mat[j]
		Escribir ''
		Escribir "CALIFICACIONES BIMESTRALES"
		para i <- 1 hasta c Con Paso 1 Hacer
			Escribir "     Bimestre ",i,": " Sin Saltar
			Escribir calf[j,i]
		FinPara
		Escribir ''
		Escribir "Promedio anual: ",prom[j]
		si j <> a Entonces
			Escribir '==========================='
		FinSi
	FinPara
FinSubProceso

///   HERRAMIENTAS   ///
//Herramienta Promedio
Funcion pr <- promedio(calf,c,j)
	//Definir  variables
	Definir i Como Entero
	Definir pr Como Real
	pr <- 0
	//Suma de bimestres
	para i <- 1 hasta c Con Paso 1 Hacer
		pr <- pr + calf[j,i]
	FinPara
	//Promedio
	pr <- REDON((pr/6)*100)/100
Fin Funcion

//Herramienta Busqueda 
SubProceso hbus(id Por Referencia,a,nom,mat,calf,prom)
	//Definir  variables
	Definir op2,m,j,i,c,switch Como Entero
	Definir n,op3 Como Caracter
	c <- 6
	Repetir
		switch <- 0
		Escribir ''
		Repetir
			//Repetir; valido para 1 y 2
			Repetir
				Escribir "Buscar por..."
				Escribir "  [1] Nombre  |  [2] Matricula"
				Leer op2
				Escribir ''
				Segun op2 hacer
					1:	Escribir "Nombre:" Sin Saltar
						Leer n
					2:	Escribir "Matricula:" Sin Saltar
						Leer m
					De Otro Modo:
						error
				FinSegun
			Hasta Que op2 <= 2 y op2 >= 1
		Hasta Que op2 >= 1 o op2 <= 2
		Escribir ''
		//Muestra de información
		para j <- 1 hasta a Con Paso 1 Hacer
			si n = nom[j] o m = mat[j] Entonces
				switch <- 1
				Escribir "  ||  ALUMNO #",j,"  ||"
				Escribir ''
				Escribir "Nombre: " Sin Saltar
				Escribir nom[j]
				Escribir "Matricula: " Sin Saltar
				Escribir mat[j]
				Escribir ''
				Escribir "CALIFICACIONES BIMESTRALES"
				para i <- 1 hasta c Con Paso 1 Hacer
					Escribir "    Bimestre ",i,": " Sin Saltar
					Escribir calf[j,i]
				FinPara
				Escribir ''
				//Promedio del alumno
				Escribir "Promedio anual: ",prom[j]
				id <- j
				j <- a
			FinSi
		FinPara
		Escribir ''
		//Aviso de alumno no registrado/encontrado
		si switch <> 1 Entonces
			Escribir "[!] ERROR: Alumno no registrado/encontrado [!]"
			Escribir ''
		FinSi
		//Confirmación 1
		Escribir "¿Buscar a alguien mas?"
		leer op3
	Hasta Que op3 <> 's' y op3 <> 'S'
FinSubProceso

///   INFORMACIÓN GENERAL   //
//MENU PRINCIPAL
SubProceso menuPrincipal(op Por Referencia)
	info
	Escribir "     -     MENU PRINCIPAL     -"
	Escribir ''
	Escribir "¿Que desea hacer?"
	Escribir "1) Dar de alta un alumno"
	Escribir "2) Dar de baja un alumno"
	Escribir "3) Modificar nombre de un alumno"
	Escribir "4) Buscar Alumno"
	Escribir "5) Mostrar alumno con mayor aprovechamiento"
	Escribir "6) Listado total de alumnos en el colegio"
	Leer op
FinSubProceso

//CABEZA INFORMACIÓN
SubProceso info
	Escribir "COLEGIO BENITO JUÁREZ   -   OAXACA"
	Escribir ''
FinSubProceso

//ERROR
SubProceso error
	Escribir "[!] ERROR: Verifica tu información [!]"
	Escribir ''
FinSubProceso

//Despedida/Creditos
SubProceso despedida
	Limpiar Pantalla
	Escribir ''
	Escribir '    -    ¡Gracias por usar nuestros servicios!    -'
	Escribir ''
	Escribir " Equipo 4                Metodologia de la Programación"
	Escribir ''
	Escribir "                                             BUAP - FCC"
	Escribir '                                             19/05/2021'
FinSubProceso

///   PROCESO PRINCIPAL   ///
Proceso colegio
	//Definir  variables
	Definir op,mat,a,id Como Entero
	Definir nom,x Como Caracter
	Definir calf,prom Como Real
	//Dimension de variables
	Dimension nom[473]
	Dimension prom[473]
	Dimension mat[473]
	Dimension calf[473,6]
	Dimension estatus[473]
	//Valores principales
	a <- 0
	Repetir
		Repetir
			Limpiar Pantalla
			menuPrincipal(op)
			//Decisiones principales
			Segun op hacer
				1:	alta(a,nom,mat,calf,prom)
				2:	baja(nom,mat,calf,prom,a,id)
				3:	modNom(id,a,nom,mat,calf,prom)
				4:	busqueda(id,a,nom,mat,calf,prom)
				5:	mejor(nom,mat,calf,prom,a)
				6:	listTotal(a,nom,mat,calf,prom)
				De Otro Modo:
					Limpiar Pantalla
					error
			FinSegun
			Escribir ''
			//Confirmación 1
			Escribir '¿Regresar al menú? (S/N)'
			leer x
		Hasta Que op <= 5 y op >= 1 
	Hasta Que x <> 's' y x <> 'S'
	despedida
FinProceso