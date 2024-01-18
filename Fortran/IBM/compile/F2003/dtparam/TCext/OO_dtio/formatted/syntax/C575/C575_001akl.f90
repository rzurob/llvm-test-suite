! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type A (kA) ! kA=4
      integer, kind :: kA
      real(kA), allocatable :: i
   end type

   type, private :: B (kB) ! kB=4
   integer, kind :: kB
      integer(kB) :: i
   end type

   type, public :: C (lC) ! lC=3
   integer, len :: lC
      character(lC), private :: cc
   end type

   type(A(4)) :: A1 ! tcx: (4)
   type(B(4)), private :: B1 ! tcx: (4)
   type(C(3)) :: C1 ! tcx: (3)

   private :: nml2, nml4

   namelist /nml1/ A1         !<- legal    public namelist , public object
   namelist /nml2/ B1         !<- legal    private namelist, private object
   namelist /nml3/ C1         !<- legal    public namelist , public object, private component
   namelist /nml4/ C1         !<- legal    private namelist, public object, private component

end module

module m2
   PRIVATE
   PUBLIC :: nml5

   type, public :: D (kD) ! kD=4
   integer, kind :: kD
      integer(kD), private :: i1
   end type

   type :: E (kE) ! kE=4
      integer, kind :: kE
      complex(kE) :: cc
   end type

   type(D(4)) :: D1 ! tcx: (4)
   type(D(4)), public :: D2 ! tcx: (4)
   type(E(4)), public :: E1 ! tcx: (4)

   namelist /nml5/  D2         !<- legal public namelist, public object, private component
   namelist /nml6/  E1         !<- legal private namelist, public object, public component
   namelist /nml7/  D1         !<- legal private namelist, private object, private component

end module

program C575_001akl
end program


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA) to invoke with (4) / declare with (4) - 1 changes
! type: B - added parameters (kB) to invoke with (4) / declare with (4) - 1 changes
! type: C - added parameters (lC) to invoke with (3) / declare with (*) - 1 changes
! type: D - added parameters (kD) to invoke with (4) / declare with (4) - 2 changes
! type: E - added parameters (kE) to invoke with (4) / declare with (4) - 1 changes
