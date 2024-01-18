! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

   type, abstract :: base
      integer :: i
   end type

   type, extends(base) :: child
      character(3) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
  end interface

   class(base), pointer :: b2(:,:)
   namelist /nml1/ b2

end module

program array001a
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   type(child)              :: b3(2,2)
   class(child), pointer    :: b4(:,:)

   namelist /nml2/ b1, b3
   namelist /nml2/ b4

   open (1, file = 'array001a.1', form='formatted', access='stream' )
   allocate(b1(2), source = (/ child(1,'abc'), child(2,'def') /))
   b3 = reshape( source = (/child(7,'ghi'), child(8,'jkl'), child(9,'mno'), child(10,'pqr')/) , shape =(/2,2/))
   allocate(b4(2,2), source = reshape( source = (/child(3,'ABC'), child(4,'DEF'), child(5,'GHI'), child(6,'JKL')/) , shape =(/2,2/)))
   allocate(b2(2,2), source = b4 )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type ( m => dtv )
      class is (base)
         error stop 5_4
      type is (child)
         write (unit, "('i=',I4,1X)", iostat=iostat )      m%i
         write (unit, "('c=',A3,1X)", iostat=iostat )      m%c
   end select

   iomsg = 'dtiowrite'

end subroutine
