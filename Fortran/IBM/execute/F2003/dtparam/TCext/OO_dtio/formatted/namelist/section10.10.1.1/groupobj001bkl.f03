! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Input data being object components of non-polymorphic array entities
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
      character(lb) :: c = 'xxx'
      integer(kb)   :: i = 999
   end type
end module

program groupobj001bkl
   use m

   integer :: stat
   character(200) :: msg
   type(base(4,3))               :: b1(3) ! tcx: (4,3)
   type(base(4,:)), allocatable  :: b2(:) ! tcx: (4,:)
   type(base(4,:)), pointer      :: b3(:,:) ! tcx: (4,:)

   namelist /nml/ b1, b2, b3
   allocate ( base(4,3):: b2(2), b3(2,2) ) ! tcx: base(4,3)

   open (1, file='groupobj001bkl.1', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)

   if ( ( b1(1)%i /= 999  )   .or. ( b1(2)%i /= 1002 )  .or. ( b1(3)%i /= 1003 ) )   error stop 1_4
   if ( ( b1(1)%c /= 'xxx'  ) .or. ( b1(2)%c /= 'xxx' ) .or. ( b1(3)%c /= 'xxx' ) )  error stop 2_4
   if ( ( b2(1)%i /= 2001  )   .or. ( b2(2)%i /= 999 )   )                           error stop 3_4
   if ( ( b2(1)%c /= 'abc'  ) .or. ( b2(2)%c /= 'xxx' )  )                           error stop 4_4
   if ( ( b3(1,1)%i /= 3001  )   .or. ( b3(2,1)%i /= 3002 )  .or. ( b3(1,2)%i /= 3003 ) .or. ( b3(2,2)%i /= 3004 ) )      error stop 5_4
   if ( ( b3(1,1)%c /= 'ABC'  ) .or. ( b3(2,1)%c /= 'DEF' ) .or. ( b3(1,2)%c /= 'GHI' ) .or. ( b3(2,2)%c /= 'JKL' ) )     error stop 6_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 3 changes
