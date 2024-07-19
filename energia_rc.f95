module rc_energy
        implicit none
        contains
        subroutine energy(energia_reactante)        
        character(len=100) :: line
        character(len=30) :: energia
        integer :: pos1, linea, largo, ene1
        logical :: archivo_existe 
        real(8) :: energia_reactante

        inquire (file='opt.log', exist=archivo_existe)
        if (archivo_existe)then
                open (unit=12, file='opt.log', status= 'old', action='read')
                do 
                        read(12,'(a100)',end=10)line
                        pos1 = index  (line,'Final converged energy:')
                        if (pos1 /=0)then
                         !write(*,*)line
                                largo= len_trim(line)
                         !write(*,*)largo
                                ene1=largo - pos1 - 24
                        ! write(*,*)ene1
                                energia=line(pos1 + 24:pos1 + 24 + ene1)
                        ! write(*,*)energia
                                read(energia,*)energia_reactante
                         !write(*,*)energia_reactante
                        endif        

                enddo 
                10 continue
                close(12)
        endif
        end subroutine energy

end module rc_energy
