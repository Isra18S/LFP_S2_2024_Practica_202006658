MODULE InventoryModule
    IMPLICIT NONE
    INTEGER, PARAMETER :: max_items = 100
    CHARACTER(LEN=50) :: item_name(max_items)
    INTEGER :: item_quantity(max_items)
    REAL :: item_price(max_items)
    CHARACTER(LEN=50) :: item_location(max_items)
    INTEGER :: count
END MODULE InventoryModule

PROGRAM InventoryManagement
    USE InventoryModule
    IMPLICIT NONE
    INTEGER :: choice

    DO
        CALL DisplayMenu(choice)
        SELECT CASE (choice)
        CASE (1)
            CALL LoadInitialInventory()
        CASE (2)
            CALL LoadMovementInstructions()
        CASE (3)
            CALL CreateInventoryReport()
        CASE (4)
            PRINT *, "Saliendo del programa..."
            EXIT
        CASE DEFAULT
            PRINT *, "Opcion no valida, por favor elija entre 1 y 4."
        END SELECT
    END DO

END PROGRAM InventoryManagement

SUBROUTINE DisplayMenu(choice)
    IMPLICIT NONE
    INTEGER :: choice
    PRINT *, "--------------------------------"
    PRINT *, " Menu de Gestion de Inventario"
    PRINT *, "--------------------------------"
    PRINT *, "1. Cargar inventario inicial"
    PRINT *, "2. Cargar instrucciones de movimientos"
    PRINT *, "3. Crear informe de inventario"
    PRINT *, "4. Salir"
    PRINT *, "Seleccione una opcion:"
    READ *, choice
END SUBROUTINE DisplayMenu

SUBROUTINE LoadInitialInventory()
    USE InventoryModule
    IMPLICIT NONE
    CHARACTER(LEN=100) :: filename
    CHARACTER(LEN=200) :: line
    CHARACTER(LEN=50) :: nombre, ubicacion
    INTEGER :: cantidad, ios, i, pos1, pos2
    REAL :: precio_unitario

    count = 0
    PRINT *, 'Ingrese el nombre del archivo de inventario inicial (.inv):'
    READ *, filename

    OPEN(UNIT=10, FILE=TRIM(filename), STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo ', TRIM(filename)
        RETURN
    ELSE
        PRINT *, 'Archivo ', TRIM(filename), ' abierto correctamente'
    END IF

    DO WHILE (.TRUE.)
        READ(10, '(A)', IOSTAT=ios) line
        IF (ios /= 0) EXIT

        IF (INDEX(line, 'crear_equipo') == 1) THEN
            ! Extraccion del nombre
            pos1 = INDEX(line, 'crear_equipo') + LEN('crear_equipo') + 1
            pos2 = INDEX(line(pos1:), ';') + pos1 - 1
            nombre = line(pos1:pos2-1)
            
            ! Extraccion de la cantidad
            pos1 = pos2 + 1
            pos2 = INDEX(line(pos1:), ';') + pos1 - 1
            READ(line(pos1:pos2-1), *) cantidad

            ! Extraccion del precio unitario
            pos1 = pos2 + 1
            pos2 = INDEX(line(pos1:), ';') + pos1 - 1
            READ(line(pos1:pos2-1), *) precio_unitario

            ! Extraccion de la ubicacion
            pos1 = pos2 + 1
            ubicacion = line(pos1:)

            count = count + 1
            item_name(count) = TRIM(nombre)
            item_quantity(count) = cantidad
            item_price(count) = precio_unitario
            item_location(count) = TRIM(ubicacion)

            ! Mostrar el inventario cargado
            PRINT *, 'Equipo:', TRIM(item_name(count))
            PRINT *, 'Cantidad:', item_quantity(count)
            PRINT *, 'Precio Unitario:', item_price(count)
            PRINT *, 'Ubicacion:', TRIM(item_location(count))
            PRINT *, '---------------------------'
        ELSE
            PRINT *, 'Instruccion no valida: ', TRIM(line)
        END IF
    END DO

    CLOSE(10)
END SUBROUTINE LoadInitialInventory

SUBROUTINE LoadMovementInstructions()
    USE InventoryModule
    IMPLICIT NONE
    CHARACTER(LEN=100) :: filename
    CHARACTER(LEN=200) :: line
    CHARACTER(LEN=50) :: nombre, ubicacion, accion
    INTEGER :: cantidad, ios, i, found, pos1, pos2
    REAL :: valor_total

    PRINT *, 'Ingrese el nombre del archivo de instrucciones de movimientos (.mov):'
    READ *, filename
    
    OPEN(UNIT=10, FILE=TRIM(filename), STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo ', TRIM(filename)
        RETURN
    END IF

    DO WHILE (.TRUE.)
        READ(10, '(A)', IOSTAT=ios) line
        IF (ios /= 0) EXIT

        ! Procesamiento manual de la linea
        IF (INDEX(line, 'agregar_stock') == 1) THEN
            accion = 'agregar'
            pos1 = INDEX(line, 'agregar_stock') + LEN('agregar_stock') + 1
        ELSE IF (INDEX(line, 'eliminar_equipo') == 1) THEN
            accion = 'eliminar'
            pos1 = INDEX(line, 'eliminar_equipo') + LEN('eliminar_equipo') + 1
        ELSE
            PRINT *, 'Instruccion no valida: ', TRIM(line)
            CYCLE
        END IF
        
        pos2 = INDEX(line(pos1:), ';') + pos1 - 1
        nombre = line(pos1:pos2-1)
        
        pos1 = pos2 + 1
        pos2 = INDEX(line(pos1:), ';') + pos1 - 1
        READ(line(pos1:pos2-1), *) cantidad
        
        pos1 = pos2 + 1
        ubicacion = line(pos1:)

        found = 0
        DO i = 1, count
            IF (TRIM(item_name(i)) == TRIM(nombre) .AND. TRIM(item_location(i)) == TRIM(ubicacion)) THEN
                found = 1
                EXIT
            END IF
        END DO
        
        IF (found == 1) THEN
            IF (accion == 'agregar') THEN
                item_quantity(i) = item_quantity(i) + cantidad
                PRINT *, 'Stock actualizado: ', TRIM(nombre), ' - Nueva cantidad: ', item_quantity(i), ' en ', TRIM(ubicacion)
            ELSE IF (accion == 'eliminar') THEN
                IF (cantidad <= item_quantity(i)) THEN
                    item_quantity(i) = item_quantity(i) - cantidad
                    PRINT *, 'Stock reducido: ', TRIM(nombre), ' - Nueva cantidad: ', item_quantity(i), ' en ', TRIM(ubicacion)
                ELSE
                    PRINT *, 'Error: No se puede eliminar mas de lo disponible para ', TRIM(nombre), ' en ', TRIM(ubicacion)
                END IF
            END IF
        ELSE
            PRINT *, 'Error: El equipo ', TRIM(nombre), ' no existe en ', TRIM(ubicacion)
        END IF
    END DO

    CLOSE(10)
END SUBROUTINE LoadMovementInstructions

! Subrutina para crear un informe del inventario
SUBROUTINE CreateInventoryReport()
    USE InventoryModule
    IMPLICIT NONE
    CHARACTER(LEN=100) :: filename
    INTEGER :: i, ios
    REAL :: valor_total

    filename = 'informe_inventario.txt'

    OPEN(UNIT=20, FILE=TRIM(filename), STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al crear el archivo ', TRIM(filename)
        RETURN
    END IF

    WRITE(20, '(A)') '-------------------------------------------'
    WRITE(20, '(A)') 'Informe de Inventario'
    WRITE(20, '(A)') '-------------------------------------------'
    WRITE(20, '(A)') 'Equipo       | Cantidad | Precio Unitario | Valor Total | Ubicacion'
    WRITE(20, '(A)') '-------------------------------------------'
    
    DO i = 1, count
        valor_total = item_quantity(i) * item_price(i)
        WRITE(20, '(A, I10, F15.2, F12.2, A)') TRIM(item_name(i)), item_quantity(i), item_price(i), valor_total, TRIM(item_location(i))
    END DO

    WRITE(20, '(A)') '-------------------------------------------'
    
    CLOSE(20)
    
    PRINT *, 'Informe de inventario creado en ', TRIM(filename)

END SUBROUTINE CreateInventoryReport
