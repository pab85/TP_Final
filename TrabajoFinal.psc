Algoritmo CentroDeportivoReservaTurnos
	// Variables para almacenado de socios	
	nrosocio <- 1
	Dimensionar socio(1,5)
	// Variables para reserva de deportes
	Dimensionar deportes(6)
	deportes[1] <- 'Natación'
	deportes[2] <- 'Gimnasio'
	deportes[3] <- 'Fronton'
	deportes[4] <- 'Fútbol'
	deportes[5] <- 'Basquet'
	deportes[6] <- 'Voley'
	Dimensionar diasAlquiler(6)
	diasAlquiler[1] <- 'Lunes'
	diasAlquiler[2] <- 'Martes'
	diasAlquiler[3] <- 'Miercoles'
	diasAlquiler[4] <- 'Jueves'
	diasAlquiler[5] <- 'Viernes'
	diasAlquiler[6] <- 'Sabado'
	Dimensionar horasAlquiler(8)
	horasAlquiler[1] <- '9 a 10 hs'
	horasAlquiler[2] <- '10 a 11 hs'
	horasAlquiler[3] <- '15 a 16 hs'
	horasAlquiler[4] <- '16 a 17 hs'
	horasAlquiler[5] <- '17 a 18 hs'
	horasAlquiler[6] <- '18 a 19 hs'
	horasAlquiler[7] <- '19 a 20 hs'
	horasAlquiler[8] <- '20 a 21 hs'
	// Variables chequear disponibilidad para reservas de deportes si es 0 esta disponible 1 ocupada
	Dimensionar deporteAlquilado(6)
	Dimensionar diaAlquilado(6)
	Dimensionar horaAlquilada(8)
	Dimensionar reservas(6,6,8)
	// Variable alquiler articulos maximo 4 
	Dimensionar articulosAlquilados(4)
	i <- 1
	Repetir
		// Ménu Principal
		Escribir 'Bienvenido, por favor ingrese la opción deseada:'
		Escribir '1. Registrarse como socio' // contador para chequear la cantidad de articulos
		Escribir '2. Reservas deportivas'
		Escribir '3. Alquiler de artículos deportivos (máximo 4 articulos)'
		Escribir '4. Salir'
		Escribir ''
		Leer opcionPrincipal
		Según opcionPrincipal Hacer
			1:
				Escribir 'Por favor ingrese su nombre:'
				Leer socio[1,1]
				Escribir 'Por favor ingrese su dirección:' // Registro de socios
				Leer socio[1,2]
				Escribir 'Por favor ingrese su teléfono:'
				Leer socio[1,3]
				Escribir 'Por favor ingrese su email:'
				Leer socio[1,4]
				socio[1,5]<-'1'
				Escribir 'Socio registrado correctamente, su número de socio es: ', nrosocio
				Escribir ''
			2:
				// Valido si el socio tiene la cuota paga
				Escribir 'Por favor indique su número de socio:'
				Leer numeroSocio
				Si socio[numeroSocio,5]='1' Entonces // Reservar para deportes
					Repetir
						alquiler <- 1
						// Elección del deporte
						// Menú de deportes disponibles
						Escribir 'Que deporte desea realizar:'
						Para j<-1 Hasta 6 Hacer
							Escribir j, '. ', deportes[j] // Para validar que no se alquile 2 veces una opción
						FinPara
						Escribir '7. No deseo realizar ningún deporte'
						Escribir ''
						Leer opcionDeporte
						Si opcionDeporte>=7 Entonces
							Si opcionDeporte=7 Entonces
								Escribir ''
								alquiler <- 0
							SiNo // Valido si quiere salir
								Escribir 'La opción seleccionada no es correcta. Por favor ingrese nuevamente.'
								Escribir ''
							FinSi // Valido que la opcion selecionada sea correcta
						SiNo
							deporteAlquilado[opcionDeporte] <- 1
							// Elección del día
							Repetir
								alquilerDia <- 1
								// Menú de días disponibles
								Escribir 'Por favor seleccione el día:' // Reservo el deporte elegido
								Para j<-1 Hasta 6 Hacer
									Escribir j, '. ', diasAlquiler[j] // Para validar que seleccione una opción del menu correcta
								FinPara
								Leer opcionDia
								Si opcionDia>6 Entonces
									Escribir 'La opción seleccionada no es correcta. Por favor ingrese nuevamente.'
									Escribir ''
								SiNo
									diaAlquilado[opcionDia] <- 1
									// Elección del horario
									Repetir
										alquilerHora <- 1
										// Menú de horas disponibles
										Escribir 'Por favor seleccione el horario:' // Reservo el día elegido
										Para j<-1 Hasta 8 Hacer
											Escribir j, '. ', horasAlquiler[j] // Para validar que seleccione una opción del menu correcta
										FinPara
										Leer opcionHora
										Si opcionHora>8 Entonces
											Escribir 'La opción seleccionada no es correcta. Por favor ingrese nuevamente.'
											Escribir ''
										SiNo // Valido que la opcion selecionada sea correcta
											// Valido que no se repita el deporte, dia y horario
											Si reservas[opcionDeporte,opcionDia,opcionHora]=1 Entonces
												Escribir 'El día y horario seleccionado se encuentra ocupado, por favor seleccione otro, muchas gracias.'
												Escribir ''
												alquilerHora <- 0
											SiNo
												reservas[opcionDeporte,opcionDia,opcionHora]<-1
												Escribir 'Usted alquilo para: ', deportes[opcionDeporte], ' el ', diasAlquiler[opcionDia], ' de ', horasAlquiler[opcionHora]
												Escribir '' // Si no se repite asigno la reserva y muestro la misma
												alquiler <- 0
											FinSi
											alquilerHora <- 0
										FinSi
									Hasta Que alquilerHora=0
									alquilerDia <- 0
								FinSi
							Hasta Que alquilerDia=0
						FinSi
					Hasta Que alquiler=0
				SiNo
					Escribir 'Su cuota social se encuentra impaga, regularice su situación y vuelva nuevamente, gracias.'
					Escribir ''
				FinSi
			3:
				// Valido si el socio tiene la cuota paga
				Escribir 'Por favor indique su número de socio:'
				Leer numeroSocio
				Si socio[numeroSocio,5]='1' Entonces // Alquiler de materiales
					Repetir
						Escribir 'Por favor seleccione la opción deseada:'
						Escribir '1.Pelota'
						Escribir '2.Raqueta'
						Escribir '3.Patas de rana'
						Escribir '4.Salir'
						Escribir ''
						Leer opcionAlquiler
						Si i<5 Y opcionAlquiler=1 Entonces
							articulosAlquilados[i] <- 'Pelota'
							i <- i+1
						FinSi
						Si i<5 Y opcionAlquiler=2 Entonces
							articulosAlquilados[i] <- 'Raqueta'
							i <- i+1
						FinSi
						Si i<5 Y opcionAlquiler=3 Entonces
							articulosAlquilados[i] <- 'Patas de rana'
							i <- i+1
						FinSi
						Si i>=5 Entonces
							Escribir 'Usted no puede alquilar mas artículos, muchas gracias'
							Escribir ''
							opcionAlquiler <- 4
						FinSi
						Si opcionAlquiler>4 Entonces
							Escribir 'La opción ingresada no es válida. Por favor ingrese la correcta, gracias.'
						FinSi
						Si opcionAlquiler=4 Entonces
							Escribir 'Usted alquilo los siguientes artículos:'
							Para j<-1 Hasta 4 Hacer
								Escribir articulosAlquilados[j]
							FinPara
							Escribir 'Muchas gracias'
							Escribir ''
						FinSi
					Hasta Que opcionAlquiler=4
				SiNo
					Escribir 'Su cuota social se encuentra impaga, regularice su situación y vuelva nuevamente, gracias.'
					Escribir ''
				FinSi
			4:
			De Otro Modo:
				Escribir 'No es una opción valida. Por favor ingrese nuevamente.'
		FinSegún // Para evitar que escriba el texto de opción invalida
	Hasta Que opcionPrincipal=4
	Escribir 'Muchas gracias por visitarnos.'
FinAlgoritmo
