module rc_energy_TZVPP
        implicit none
        contains
        subroutine energy_TZVPP(energia_reactante_TZVPP)

!program TZVPP_energy
!        implicit none
!        
        character(len=100) :: line
        character(len=30) :: energia
        character(len=30) :: linea_energia
        character(len=30) :: energy_stri
        integer :: pos1
        logical :: archivo_existe
!
        real(8) :: energia_reactante_TZVPP
        inquire(file='opt.log', exist=archivo_existe)
        if (archivo_existe)then
                open (unit=13, file='opt.log', status= 'old', action='read') 
                do 
                        read(13,'(a100)',end=10)line
                 !pos1 = index  (line,'Energy (     hybrid):')
                        pos1 = index  (line,'QM/MM Energy:')
                        if (pos1 /=0)then
!                         write(*,*)line
                         !read(line(65:80),'(f12.6)') energy
                        read(line,'(64x,f11.6)') energia_reactante_TZVPP
                         !write(*,*) energia_reactante_TZVPP
                        endif        
!
                enddo 
                10 continue
                close(13)
        !else 
                !print *,'archivo no existe'
        endif
        end subroutine energy_TZVPP
end module rc_energy_TZVPP

!program TZVPP_energy
!        implicit none
!
!        character(len=100) :: line
!        character(len=30) :: energia
!        integer :: pos1, linea, largo, posicion
!
!        real :: energy
!        open (unit=13, file='opt.log', status= 'old', action='read')
!
!        do
!                 read(13,'(a100)',end=10)line
!                 pos1 = index  (line,'Energy (     hybrid):')
!                 if (pos1 /=0)then
!                         !write(*,*)line
!                         largo= len_trim(line)
!                         !write(*,*)largo
!                         posicion=largo - pos1 - 47
!                         !write(*,*)ene1
!                          energia=trim(line(pos1 + 47:pos1 + 47 + posicion))
!                          energia=adjustl(energia)
!                          write(*,*) energia
!                          !read(energia,*)energy
!                          !write(*,*)energy
!                 endif
!
!        enddo
!        10 continue
!        close(12)
!end program TZVPP_energy

