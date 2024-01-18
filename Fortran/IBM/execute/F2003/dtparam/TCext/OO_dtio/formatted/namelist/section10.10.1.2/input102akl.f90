! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : input102akl
!*
!*  PROGRAMMER                 : David Forster (derived from input102a by Robert Ma)
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (no dtio procedure involved, with class hierarchy)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base (kb) ! kb=4
      integer, kind :: kb
      integer(kb)   :: i = -999
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      real(kc)      :: r = -9.9
   end type

   type, extends(child) :: gen3 (lg) ! lg=3
      integer, len :: lg
      character(lg) :: c = 'xyz'
   end type
end module

program input102akl
   use m

   integer :: stat
   character(150) :: msg
   procedure(logical) :: precision_r4
   type(child(4,4))               :: b1 ! tcx: (4,4)
   type(child(4,4)), pointer      :: b2 ! tcx: (4,4)
   type(gen3(4,4,:)), allocatable   :: b3 ! tcx: (4,4,:)
   type(gen3(4,4,3))                :: b4 ! tcx: (4,4,3)

   namelist /n1/ b1, b2
   namelist /n1/ b3, b4  !<- namelist contains 4 items

   allocate(b2)
   allocate(gen3(4,4,3):: b3) ! tcx: gen3(4,4,3)

   open (1, file='input102akl.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)

   if ( ( b1%i /= 12345 ) .or. ( .not. precision_r4(b1%r,-9.9)) )    error stop 1_4
   if ( ( b2%i /= 5     ) .or. ( .not. precision_r4(b2%r,-0.1234)) ) error stop 2_4
   if ( ( b3%i /= -9876 ) .or. ( .not. precision_r4(b3%r,-9.9 )) .or. ( b3%c /= 'ibm' ) ) error stop 3_4  !<- no change in b3%c
   if ( ( b4%i /= -999 ) .or. ( .not. precision_r4(b4%r,-9.9 )) .or. ( b4%c /= 'IBM' ) ) error stop 4_4  !<- no change in b4%i, b4%r

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 0 changes
! type: base - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 0 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 2 changes
! type: gen3 - added parameters (lg) to invoke with (4,4,3) / declare with (4,4,*) - 2 changes
