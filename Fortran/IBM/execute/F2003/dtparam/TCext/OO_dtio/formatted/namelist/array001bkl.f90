! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001bkl
!*
!*  PROGRAMMER                 : David Forster (derived from array001b by Robert Ma)
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array with type with derived type component (Output)
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

   type :: data (kd)
      integer, kind :: kd
      integer(kd) :: i, j
   end type

   type :: base (kb)
      integer, kind :: kb
      type(data(kb)) :: d1
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      type(data(kc)) :: d2
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
  end interface

   class(base(4)), pointer :: b2(:,:)
   namelist /nml1/ b2

end module

program array001bkl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1(:)
   type(child(4,4))              :: b3(2,2)
   class(child(4,4)), pointer    :: b4(:,:)

   namelist /nml2/ b1, b3
   namelist /nml2/ b4

   open (1, file = 'array001bkl.1', form='formatted', access='stream' )
   allocate(b1(2), source = (/ child(4,4)(data(4)(1001,1002), data(4)(1003,1004)), child(4,4)(data(4)(1005,1006), data(4)(1007,1008)) /))
   b3 = reshape( source = (/ child(4,4)(data(4)(1009,1010), data(4)(1011,1012)), child(4,4)(data(4)(1013,1014), data(4)(1015,1016)),  &
                             child(4,4)(data(4)(1017,1018), data(4)(1019,1020)), child(4,4)(data(4)(1021,1022), data(4)(1023,1024))/) , shape =(/2,2/))
   allocate(b4(2,2), source = b3 )
   allocate(b2(2,2), source = reshape ( source = (/ b1, b1 /) , shape = (/ 2, 2 /) )  )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type ( m => dtv )
      class is (base(4))
         write (unit, *, iostat=iostat )      m%d1
      type is (child(4,4))
         write (unit, *, iostat=iostat )      m%d1
         write (unit, *, iostat=iostat )      m%d2
   end select

   iomsg = 'dtiowrite'

end subroutine
