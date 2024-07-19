program list_directories
    implicit none
    integer, parameter :: max_dir = 100
    character(len=256) :: dir_nombre(max_dir)
    integer :: i, numero_dirs, ios
    character(len=256) :: linea
    character(len=256) :: archivo, archivo_salida, archivo_nuevo
    logical :: archivo_existe

    archivo = 'directorios.txt'
    archivo_salida = 'salida.txt'
    archivo_nuevo = 'resultados.txt'
    call system('ls -d */ | grep -E "^[0-9]{2}_[0-9]+ns_[0-9]+/$" > ' // trim(archivo))

    inquire(file=archivo, exist=archivo_existe)
    if (.not. archivo_existe) then
        print *, 'no se encuentra el: ', archivo
        stop
    end if

    open(unit=10, file=archivo, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'no se abrio el', archivo
        stop
    end if

    numero_dirs = 0  
    
    do i = 1, max_dir
        read(10, '(a)', iostat=ios) linea
        if (ios /= 0) exit
        numero_dirs = numero_dirs + 1
        dir_nombre(numero_dirs) = adjustl(linea)
    end do

    close(10)
    
    open(unit=20, file=archivo_nuevo,status='replace', action='write')
    write(20, '(A)') 'snapshot                  TS        P'

    do i = 1, numero_dirs
        call procesar_directorio(trim(dir_nombre(i)), i,20)
    end do

    close(20)
    
    call system('rm -f ' // trim(archivo))
    call system('rm -f ' // archivo_salida)
    
    print *,'Archivo de energias creado ;)'


contains

    subroutine procesar_directorio(directorio,numero_dirs,salida_unit)
        implicit none
        character(len=*), intent(in) :: directorio
        character(len=256) :: full_path
        character(len=256) :: comando
        integer :: indice
        character(len=256) :: linea
        character(len=256) :: line
        real :: TS_value, P_value
        integer :: line_TS, line_PROD
        logical :: found_TS, found_PROD
        character(len=256) :: TS_line, PROD_line
        integer, intent(in) :: numero_dirs, salida_unit
        integer :: j,i

        full_path = trim(directorio)
        comando = 'cd ' // trim(directorio) //  ' &&  ../extraccion'
        print *, 'entrando en el directorio: ', trim(full_path)
        call system(comando)

        archivo_salida = 'salida.txt'

        found_TS = .false.
        found_PROD = .false.
        line_TS = 3 + (numero_dirs - 1) * 4
        line_PROD = 5 + (numero_dirs - 1) * 4

        open(unit=30, file=archivo_salida, status='old', action='read', iostat=ios)
        
        if (ios /= 0) then
            print *, 'No se pudo abrir el archivo ', trim(archivo_salida)
            return
        end if

        do i = 1, line_TS - 1
                read(30, '(A)', iostat=ios)
                if (ios /= 0) exit
        end do


        if (ios == 0) then
                read(30, '(90X, F10.6)', iostat=ios) TS_value
                if (ios/=0)then
                        TS_value=0.0
                else
                        found_TS =.true.
                endif 
        else
             TS_value=0.0   
        end if

        do i = line_TS, line_PROD,-1 
                read(30, '(A)', iostat=ios)
                if (ios /= 0) exit
                !print *, 'Línea ', i, ': ', trim(linea)  
        end do

        if (ios == 0) then
                read(30, '(90X, F10.6)', iostat=ios) P_value
                if (ios/=0)then
                        print * , 'error verifica prod'
                        P_value=0.0
                else
                        found_PROD = .true.
                endif
        else
                P_value=0.0
        end if
        write(salida_unit, '(A16,2x,F10.1,2x,F10.1)') trim(directorio), TS_value, P_value
        close(30)
    end subroutine procesar_directorio

end program list_directories

