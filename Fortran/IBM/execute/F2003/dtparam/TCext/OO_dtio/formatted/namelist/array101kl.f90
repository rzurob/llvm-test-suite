! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array101kl
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array (Input)
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
   type base (kb)
      integer, kind :: kb
      integer(kb) :: i = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), pointer :: b2(:,:)

end module

program array101kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1(:,:)
   type(base(4))               :: b3(2,2)
   type(base(4)), allocatable  :: b4(:,:)
   type(base(4)), pointer      :: b5(:,:)

   namelist /nml1/ b1, b2
   namelist /nml2/ b3
   namelist /nml3/ b4, b5

   open (1, file = 'array101kl.1', form='formatted', access='stream' )
   allocate(b1(2,2), b2(3,3) )
   b3 = base(4)()
   allocate(b4(2,2), b5(3,3))

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( ( b1(1,1)%i /= 101 ) .or. ( b1(2,1)%i /= 102 ) .or. ( b1(1,2)%i /= 103 ) .or. ( b1(2,2)%i /= 104 ) ) error stop 4_4
   if ( ( b2(1,1)%i /= 201 ) .or. ( b2(2,1)%i /= 202 ) .or. ( b2(3,1)%i /= 203 ) .or.  &
        ( b2(1,2)%i /= 204 ) .or. ( b2(2,2)%i /= 205 ) .or. ( b2(3,2)%i /= 206 ) .or.  &
        ( b2(1,3)%i /= 207 ) .or. ( b2(2,3)%i /= 208 ) .or. ( b2(3,3)%i /= 209 ) ) error stop 5_4
   if ( ( b3(1,1)%i /= 301 ) .or. ( b3(2,1)%i /= 302 ) .or. ( b3(1,2)%i /= 303 ) .or. ( b3(2,2)%i /= 304 ) ) error stop 6_4
   if ( ( b4(1,1)%i /= 401 ) .or. ( b4(2,1)%i /= 402 ) .or. ( b4(1,2)%i /= 403 ) .or. ( b4(2,2)%i /= 404 ) ) error stop 7_4
   if ( ( b5(1,1)%i /= 501 ) .or. ( b5(2,1)%i /= 502 ) .or. ( b5(3,1)%i /= 503 ) .or.  &
        ( b5(1,2)%i /= 504 ) .or. ( b5(2,2)%i /= 505 ) .or. ( b5(3,2)%i /= 506 ) .or.  &
        ( b5(1,3)%i /= 507 ) .or. ( b5(2,3)%i /= 508 ) .or. ( b5(3,3)%i /= 509 ) ) error stop 8_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   read (unit, *, iostat=iostat, iomsg = iomsg )      dtv%i
   iomsg = 'dtioread'

end subroutine
