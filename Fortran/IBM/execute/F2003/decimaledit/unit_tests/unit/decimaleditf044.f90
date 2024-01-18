!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditf044.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : decimaleditf044
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb. 09, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the decimal
!*                               edit mode in Fortran 2003 std ( Feature
!*                               289039 ). This feature affects the decimal
!*                               symbol and value separator during I/O.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : namelist output for logical/real/complex/integer
!*                               with DECIAMl= specifier. This tests internal file i/o.
!*                               ( compile-time encoding )
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, parameter :: OUT = 22 ! unit numbers

      real :: rlarr(5) = (/1.11,22.2,333.33,44.4,0.0/)
      integer :: iarr(2) = (/123,456/)
      logical :: larr(3) = (/.true.,.false.,.true./)
      complex :: cxarr(2) = (/(7.7,8.8),(9.9,6.6)/)

      character(170) :: buffer(3)
      
      namelist /nml_all/ rlarr, iarr, larr, cxarr
      namelist /nml_r/ rlarr
      namelist /nml_i/ iarr
      namelist /nml_l/ larr
      namelist /nml_cx/ cxarr
      
      open(unit=OUT, file='decimaleditf044.out')

      write(buffer,nml_r,decimal='comma')
      write(OUT,*) buffer
      write(buffer,nml_i,decimal='comma')
      write(OUT,*) buffer
      write(buffer,nml_l,decimal='comma')
      write(OUT,*) buffer
      write(buffer,nml_cx,decimal='comma')
      write(OUT,*) buffer
      write(buffer,nml_all,decimal='comma')
      write(OUT,*) buffer

      write(buffer,nml_r,decimal='point')
      write(OUT,*) buffer
      write(buffer,nml_i,decimal='point')
      write(OUT,*) buffer
      write(buffer,nml_l,decimal='point')
      write(OUT,*) buffer
      write(buffer,nml_cx,decimal='point')
      write(OUT,*) buffer
      write(buffer,nml_all,decimal='point')
      write(OUT,*) buffer

      end
