! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : input102kl
!*
!*  PROGRAMMER                 : David Forster (derived from input102 by Robert Ma)
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
!*                                       (no dtio procedure involved)
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
   type base (kb,lb) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb
      integer(kb)   :: i = -999
      real(kb)      :: r = -9.9
      character(lb) :: c = 'xyz'
   end type
end module

program input102kl
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4
   type(base(4,3))               :: b1 ! tcx: (4,3)
   type(base(4,:)), pointer      :: b2 ! tcx: (4,:)
   type(base(4,:)), allocatable  :: b3 ! tcx: (4,:)

   namelist /n1/ b1, b2
   namelist /n1/ b3, b1  !<- namelist contains 4 items

   allocate(base(4,3):: b2,b3) ! tcx: base(4,3)

   open (1, file='input102kl.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4
   if ( ( b1%i /= 12345 ) .or. ( .not. precision_r4(b1%r,8.23456)) .or. ( b1%c /= 'ibm' ) ) error stop 2_4
   if ( ( b2%i /= 5     ) .or. ( .not. precision_r4(b2%r,-0.0023)) .or. ( b2%c /= 'I M' ) ) error stop 3_4
   if ( ( b3%i /= -7825 ) .or. ( .not. precision_r4(b3%r,0.2006 )) .or. ( b3%c /= 'xyz' ) ) error stop 4_4  !<- no change in b3%c

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 3 changes
