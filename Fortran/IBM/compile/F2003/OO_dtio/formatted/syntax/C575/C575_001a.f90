!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: C575_001a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

   type(A) :: A1
   type(B), private :: B1
   type(C) :: C1

   private :: nml2, nml4

   namelist /nml1/ A1         !<- legal    public namelist , public object
   namelist /nml2/ B1         !<- legal    private namelist, private object
   namelist /nml3/ C1         !<- legal    public namelist , public object, private component
   namelist /nml4/ C1         !<- legal    private namelist, public object, private component

end module

module m2
   PRIVATE
   PUBLIC :: nml5

   type, public :: D
      integer, private :: i1
   end type

   type :: E
      complex :: cc
   end type

   type(D) :: D1
   type(D), public :: D2
   type(E), public :: E1

   namelist /nml5/  D2         !<- legal public namelist, public object, private component
   namelist /nml6/  E1         !<- legal private namelist, public object, public component
   namelist /nml7/  D1         !<- legal private namelist, private object, private component

end module

program C575_001a
end program
