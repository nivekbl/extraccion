program algo 
use rc_energy
use rc_energy_TZVPP
       implicit none
       real(8) :: energia , rc , rc_TZVPP,energia_TZVPP, ts , ts_TZVPP, prod, prod_TZVPP
       real(8) :: rc_convert , rc_TZVPP_convert, ts_convert , ts_TZVPP_convert, prod_convert, prod_TZVPP_convert
       character(len=20) :: basis 
       CHARACTER(len=200) :: dir_actual
       CHARACTER(len=200) :: archivo
       CHARACTER(len=200) :: dir_gs,dir_ts,dir_prod,dir_TZVPP
       character(len=20) :: tabla_texto(4,5) 
       logical :: dir_gs_existe
       logical :: dir_ts_existe
       logical :: dir_prod_existe
       logical :: dir_TZVPP_existe
       logical :: file_exist
       integer :: i ,output_unit, ios

       real(8) :: tabla_valor(4,5)
      
       dir_gs = 'gs'
       dir_ts = 'ts'
       dir_prod = 'prod'
       dir_TZVPP = 'TZVPP'
       call iniciar_tabla(tabla_texto, tabla_valor)
                 do 
                        INQUIRE(file=trim(dir_gs), exist=dir_gs_existe)
                        if(.not. dir_gs_existe)then
                                rc=0
                                tabla_texto(2,1)= 'RC'
                                tabla_valor(2,2)= rc
                                rc_convert=0
                                tabla_valor(2,3)= rc_convert
                                rc_TZVPP=0
                                tabla_valor(2,4) = rc_TZVPP
                                rc_convert=0
                                tabla_valor(2,5)= rc_convert
                                write(*,'(a)')'Ojo que hay un problema con el directorio gs'
                               
                        else 
                                call chdir(dir_gs)
                                call energy(energia)
                                rc=energia
                                tabla_texto(2,1)= 'RC'
                                tabla_valor(2,2)= rc
                                rc_convert=0
                                tabla_valor(2,3)= rc_convert
                        
                                 INQUIRE(file=trim(dir_TZVPP), exist=dir_TZVPP_existe)
                                 if(.not. dir_TZVPP_existe)then
                                        rc_TZVPP=0
                                        tabla_valor(2,4) = rc_TZVPP
                                        rc_convert=0
                                        tabla_valor(2,5)= rc_convert
                                 else
                                        call chdir(dir_TZVPP)
                                        call energy_TZVPP(energia_TZVPP)
                                        rc_TZVPP=energia_TZVPP
                                        tabla_valor(2,4) = rc_TZVPP
                                        rc_convert=0
                                        tabla_valor(2,5)= rc_convert
                                 endif
                        call chdir('..')
                        call chdir('..')

                         endif
                
                        INQUIRE(file=trim(dir_ts), exist=dir_ts_existe)
                        if(.not. dir_ts_existe)then
                                ts=0
                                tabla_texto(3,1)= 'TS'
                                tabla_valor(3,2) = ts
                                ts_convert=0
                                tabla_valor(3,3) = ts_convert
                                ts_TZVPP=0
                                tabla_valor(3,4) = ts_TZVPP
                                ts_TZVPP_convert=0
                                tabla_valor(3,5) = ts_TZVPP_convert
                                write(*,'(a)')'Ojo que hay un problema con el directorio ts'
                        else
                                call chdir(dir_ts)
                                call energy(energia)
                                ts=energia
                                tabla_texto(3,1)= 'TS'
                                tabla_valor(3,2) = ts
                                ts_convert= (ts-rc) * 627.5095
                                tabla_valor(3,3) = ts_convert

                                INQUIRE(file=trim(dir_TZVPP), exist=dir_TZVPP_existe)
                                 if(.not. dir_TZVPP_existe)then
                                         ts_TZVPP=0
                                         tabla_valor(3,4) = ts_TZVPP
                                         ts_TZVPP_convert= (ts_TZVPP-rc_TZVPP) * 627.5095
                                         tabla_valor(3,5) = ts_TZVPP_convert
                                 else
                                         call chdir(dir_TZVPP)
                                         call energy_TZVPP(energia_TZVPP)
                                         ts_TZVPP=energia_TZVPP
                                         tabla_valor(3,4) = ts_TZVPP
                                         ts_TZVPP_convert= (ts_TZVPP-rc_TZVPP) * 627.5095
                                         tabla_valor(3,5) = ts_TZVPP_convert
                                 endif
                         endif

                                
                        call chdir('..')
                        call chdir('..')
                        INQUIRE(file=trim(dir_prod), exist=dir_prod_existe)
                        if(.not. dir_prod_existe)then
                                prod=0
                                tabla_texto(4,1)= 'PROD'
                                tabla_valor(4,2) = prod
                                prod_convert=0
                                tabla_valor(4,3) = prod_convert
                                prod_TZVPP=0
                                tabla_valor(4,4) = prod_TZVPP
                                prod_TZVPP_convert=0
                                tabla_valor(4,5) = prod_TZVPP_convert
                                write(*,'(a)')'Ojo que hay un problema con el directorio prod'
                        else
                                call chdir(dir_prod)
                                call energy(energia)
                                prod=energia
                                tabla_texto(4,1)= 'PROD'
                                tabla_valor(4,2) = prod
                                prod_convert= (prod-rc) * 627.5095
                                tabla_valor(4,3) = prod_convert

                                INQUIRE(file=trim(dir_TZVPP), exist=dir_TZVPP_existe)
                                 if(.not. dir_TZVPP_existe)then
                                         prod_TZVPP=0
                                         tabla_valor(4,4) = prod_TZVPP
                                         prod_TZVPP_convert=0
                                         tabla_valor(4,5) = prod_TZVPP_convert
                                 else
                                        call chdir(dir_TZVPP)
                                        call energy_TZVPP(energia_TZVPP)
                                        prod_TZVPP=energia_TZVPP
                                        tabla_valor(4,4) = prod_TZVPP
                                        prod_TZVPP_convert= (prod_TZVPP-rc_TZVPP) * 627.5095
                                        tabla_valor(4,5) = prod_TZVPP_convert

                                 endif
                         endif
                         call chdir('..')
                         call chdir('..')
                         call chdir('..')
                        exit
                 enddo
                 
                archivo ='salida.txt'

                open(newunit=output_unit, file=trim(archivo), status='unknown', action='write', position='append', iostat=ios)
                if (ios /= 0) then
                         print *, "Error al abrir el archivo de salida. Código de error: ", ios
                stop
                endif

   
              call imprimir_tabla(tabla_texto, tabla_valor,output_unit)
              close(output_unit)
              inquire(file=Archivo, exist=file_exist)
              if (file_exist) then
              else
                      print *, "El archivo 'salida.txt' no se ha encontrado."
              endif

              
                    
contains


    subroutine iniciar_tabla(tabla_texto, tabla_valor)
        character(len=20), intent(out) :: tabla_texto(4, 5)
        real(8), intent(out) :: tabla_valor(4, 5)
        integer :: i, j
        do i = 1, 4
            do j = 1, 5
                tabla_texto(i, j) = ''
                tabla_valor(i, j) = 0.0
            end do
        end do
    end subroutine iniciar_tabla

    subroutine imprimir_tabla(tabla_texto, tabla_valor,output_unit)
        character(len=20), intent(in) :: tabla_texto(4, 5)
        real(8), intent(in) :: tabla_valor(4, 5)
        integer, intent(in) :: output_unit
        integer :: i, j
        write(*, '(A20, 4A20)') 'Estado', 'def2-SVP(u.a)', 'kcal/mol', 'def2-TZVPP(u.a)', 'kcal/mol'
        do i = 2, 4
             write(*, '(A20, 2F20.6, F20.6, F20.6)') trim(tabla_texto(i, 1)), &
                                                     tabla_valor(i, 2), tabla_valor(i, 3), &
                                                     tabla_valor(i, 4), tabla_valor(i, 5)
        end do
        write(output_unit, '(A20, 4A20)') 'Estado', 'def2-SVP', 'kcal/mol', 'def2-TZVPP', 'kcal/mol'
        do i = 2, 4
            write(output_unit, '(A20, 2F20.6, F20.6, F20.6)') trim(tabla_texto(i, 1)), &
                                                                tabla_valor(i, 2), tabla_valor(i, 3), &
                                                                tabla_valor(i, 4), tabla_valor(i, 5)
        enddo
    end subroutine imprimir_tabla
end program algo
