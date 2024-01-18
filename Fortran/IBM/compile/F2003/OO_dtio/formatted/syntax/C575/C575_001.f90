!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp C575_001.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: C575
!*                                        A namelist group object shall not
!*                                        have the PRIVATE attribute if the
!*                                        namelist group name has the PUBLIC attribute
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type A
      real(4), allocatable :: i
   end type

   type, private :: B
      integer(4) :: i
   end type

   type, public :: C
      character(3), private :: cc
   end type

   type(A), private :: A1
   type(B), private :: B1
   type(C), private :: C1

   namelist /nml1/ A1         !<- illegal public namelist, private object, public comp
   namelist /nml2/ B1         !<- illegal public namelist, private object, public comp
   namelist /nml3/ C1         !<- illegal public namelist, private object, private comp

end module

module m2
   PRIVATE
   PUBLIC :: nml4, nml5

   type, public :: D
      integer, public :: i1
   end type

   type :: E
      complex :: cc
   end type

   type(D) :: D1
   type(E) :: E1

   namelist /nml4/  D1         !<- illegal public namelist, private object
   namelist /nml5/  E1         !<- illegal public namelist, private object

end module

program C575_001
end program
