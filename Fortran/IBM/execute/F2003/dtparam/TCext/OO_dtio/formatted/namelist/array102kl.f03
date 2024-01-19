! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-25 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting implicit array objects (Input)
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

   type :: base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      character(lb) ::  c = 'xxx'
      integer(kb)   ::  i = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array102kl
   use m
   implicit type(base(4,3))   (A-M)
   implicit class(base(4,:))  (N-Z)

   namelist /nml1/ b1, b2
   namelist /nml2/ z3, z4

   dimension :: b1(2)
   allocatable  :: b2(:)
   pointer  :: z3(:)
   allocatable :: z4(:,:)

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'array102kl.1', form='formatted', access='sequential' )

   allocate (b2(2))
   allocate(base(4,3):: z3(4))
   allocate(base(4,3):: z4(2,2))

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 1001 ) .or. ( b1(2)%c /= 'xxx' ) .or. ( b1(2)%i /= -999 ) ) error stop 3_4
   if ( ( b2(1)%c /= 'ABC' ) .or. ( b2(1)%i /= 2001 ) .or. ( b2(2)%c /= 'DEF' ) .or. ( b2(2)%i /= 2002 ) ) error stop 4_4
   if ( ( z3(1)%c /= 'abc' ) .or. ( z3(1)%i /= 3001 ) .or. ( z3(2)%c /= 'def' ) .or. ( z3(2)%i /= 3002 ) .or.  &
        ( z3(3)%c /= 'xxx' ) .or. ( z3(3)%i /= -999 ) .or. ( z3(4)%c /= 'xxx' ) .or. ( z3(4)%i /= -999 ) ) error stop 5_4
   if ( ( z4(1,1)%c /= 'abc' ) .or. ( z4(1,1)%i /= 4001 ) .or. ( z4(2,1)%c /= 'xxx' ) .or. ( z4(2,1)%i /= -999 ) .or.  &
        ( z4(1,2)%c /= 'xxx' ) .or. ( z4(1,2)%i /= -999 ) .or. ( z4(2,2)%c /= 'xyz' ) .or. ( z4(2,2)%i /= 4004 ) ) error stop 6_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read (unit, *, iostat=iostat )        dtv%i, dtv%c

   iomsg = 'dtioread'

end subroutine
