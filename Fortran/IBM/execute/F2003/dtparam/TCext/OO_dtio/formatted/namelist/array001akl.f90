! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001akl
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array with abstract type with intrinsic components (Output)
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

   type, abstract :: base (kb)
      integer, kind :: kb
      integer(kb) :: i
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: c
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

program array001akl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1(:)
   type(child(4,3))              :: b3(2,2)
   class(child(4,:)), pointer    :: b4(:,:)

   namelist /nml2/ b1, b3
   namelist /nml2/ b4

   open (1, file = 'array001akl.1', form='formatted', access='stream' )
   allocate(b1(2), source = (/ child(4,3)(1,'abc'), child(4,3)(2,'def') /))
   b3 = reshape( source = (/child(4,3)(7,'ghi'), child(4,3)(8,'jkl'), child(4,3)(9,'mno'), child(4,3)(10,'pqr')/) , shape =(/2,2/))
   allocate(b4(2,2), source = reshape( source = (/child(4,3)(3,'ABC'), child(4,3)(4,'DEF'), child(4,3)(5,'GHI'), child(4,3)(6,'JKL')/) , shape =(/2,2/)))
   allocate(b2(2,2), source = b4 )

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
         error stop 5_4
      type is (child(4,*))
         write (unit, "('i=',I4,1X)", iostat=iostat )      m%i
         write (unit, "('c=',A3,1X)", iostat=iostat )      m%c
   end select

   iomsg = 'dtiowrite'

end subroutine
