Algoritmo CentroDeportivoReservaTurnos
	// Variables para almacenado de socios	
	nrosocio <- 1
	Dimensionar socio(1,5)
	// Variables para reserva de deportes
	Dimensionar deportes(6)
	deportes[1] <- 'Nataci�n'
	deportes[2] <- 'Gimnasio'
	deportes[3] <- 'Fronton'
	deportes[4] <- 'F�tbol'
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
		// M�nu Principal
		Escribir 'Bienvenido, por favor ingrese la opci�n deseada:'
		Escribir '1. Registrarse como socio' // contador para chequear la cantidad de articulos
		Escribir '2. Reservas deportivas'
		Escribir '3. Alquiler de art�culos deportivos (m�ximo 4 articulos)'
		Escribir '4. Salir'
		Escribir ''
		Leer opcionPrincipal
		Seg�n opcionPrincipal Hacer
			1:
				Escribir 'Por favor ingrese su nombre:'
				Leer socio[1,1]
				Escribir 'Por favor ingrese su direcci�n:' // Registro de socios
				Leer socio[1,2]
				Escribir 'Por favor ingrese su tel�fono:'
				Leer socio[1,3]
				Escribir 'Por favor ingrese su email:'
				Leer socio[1,4]
				socio[1,5]<-'1'
				Escribir 'Socio registrado correctamente, su n�mero de socio es: ', nrosocio
				Escribir ''
			2:
				// Valido si el socio tiene la cuota paga
				Escribir 'Por favor indique su n�mero de socio:'
				Leer numeroSocio
				Si socio[numeroSocio,5]='1' Entonces // Reservar para deportes
					Repetir
						alquiler <- 1
						// Elecci�n del deporte
						// Men� de deportes disponibles
						Escribir 'Que deporte desea realizar:'
						Para j<-1 Hasta 6 Hacer
							Escribir j, '. ', deportes[j] // Para validar que no se alquile 2 veces una opci�n
						FinPara
						Escribir '7. No deseo realizar ning�n deporte'
						Escribir ''
						Leer opcionDeporte
						Si opcionDeporte>=7 Entonces
							Si opcionDeporte=7 Entonces
								Escribir ''
								alquiler <- 0
							SiNo // Valido si quiere salir
								Escribir 'La opci�n seleccionada no es correcta. Por favor ingrese nuevamente.'
								Escribir ''
							FinSi // Valido que la opcion selecionada sea correcta
						SiNo
							deporteAlquilado[opcionDeporte] <- 1
							// Elecci�n del d�a
							Repetir
								alquilerDia <- 1
								// Men� de d�as disponibles
								Escribir 'Por favor seleccione el d�a:' // Reservo el deporte elegido
								Para j<-1 Hasta 6 Hacer
									Escribir j, '. ', diasAlquiler[j] // Para validar que seleccione una opci�n del menu correcta
								FinPara
								Leer opcionDia
								Si opcionDia>6 Entonces
									Escribir 'La opci�n seleccionada no es correcta. Por favor ingrese nuevamente.'
									Escribir ''
								SiNo
									diaAlquilado[opcionDia] <- 1
									// Elecci�n del horario
									Repetir
										alquilerHora <- 1
										// Men� de horas disponibles
										Escribir 'Por favor seleccione el horario:' // Reservo el d�a elegido
										Para j<-1 Hasta 8 Hacer
											Escribir j, '. ', horasAlquiler[j] // Para validar que seleccione una opci�n del menu correcta
										FinPara
										Leer opcionHora
										Si opcionHora>8 Entonces
											Escribir 'La opci�n seleccionada no es correcta. Por favor ingrese nuevamente.'
											Escribir ''
										SiNo // Valido que la opcion selecionada sea correcta
											// Valido que no se repita el deporte, dia y horario
											Si reservas[opcionDeporte,opcionDia,opcionHora]=1 Entonces
												Escribir 'El d�a y horario seleccionado se encuentra ocupado, por favor seleccione otro, muchas gracias.'
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
					Escribir 'Su cuota social se encuentra impaga, regularice su situaci�n y vuelva nuevamente, gracias.'
					Escribir ''
				FinSi
			3:
				// Valido si el socio tiene la cuota paga
				Escribir 'Por favor indique su n�mero de socio:'
				Leer numeroSocio
				Si socio[numeroSocio,5]='1' Entonces // Alquiler de materiales
					Repetir
						Escribir 'Por favor seleccione la opci�n deseada:'
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
							Escribir 'Usted no puede alquilar mas art�culos, muchas gracias'
							Escribir ''
							opcionAlquiler <- 4
						FinSi
						Si opcionAlquiler>4 Entonces
							Escribir 'La opci�n ingresada no es v�lida. Por favor ingrese la correcta, gracias.'
						FinSi
						Si opcionAlquiler=4 Entonces
							Escribir 'Usted alquilo los siguientes art�culos:'
							Para j<-1 Hasta 4 Hacer
								Escribir articulosAlquilados[j]
							FinPara
							Escribir 'Muchas gracias'
							Escribir ''
						FinSi
					Hasta Que opcionAlquiler=4
				SiNo
					Escribir 'Su cuota social se encuentra impaga, regularice su situaci�n y vuelva nuevamente, gracias.'
					Escribir ''
				FinSi
			4:
			De Otro Modo:
				Escribir 'No es una opci�n valida. Por favor ingrese nuevamente.'
		FinSeg�n // Para evitar que escriba el texto de opci�n invalida
	Hasta Que opcionPrincipal=4
	Escribir 'Muchas gracias por visitarnos.'
FinAlgoritmo
