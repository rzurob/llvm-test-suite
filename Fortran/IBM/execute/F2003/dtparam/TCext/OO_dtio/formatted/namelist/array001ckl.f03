! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array with abstract type with polymorphic derived type component(Output)
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
      integer(kd) :: i
   end type

   type, extends(data) :: childdata (lcd)
      integer, len :: lcd
      character(lcd) :: c
   end type

   type, abstract :: base (kb)
      integer, kind :: kb
      class(data(kb)), pointer     :: d1
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      class(data(kc)), allocatable :: d2
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

   class(child(4,4)), pointer :: b2(:,:)
   namelist /nml1/ b2

end module

program array001ckl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1(:)
   type(child(4,4))              :: b3(2,2)
   class(child(4,4)), pointer    :: b4(:,:)

   namelist /nml2/ b1, b3
   namelist /nml2/ b4

   open (1, file = 'array001ckl.1', form='formatted', access='stream' )
   allocate ( child(4,4) :: b1(2), b2(2,2) )
   select type ( b1 )
      type is (child(4,4))
         allocate ( b1(1)%d1, source = childdata(4,1)(1,'a') )
         allocate ( b1(1)%d2, source = childdata(4,1)(2,'b') )
         allocate ( b1(2)%d1, source = childdata(4,1)(3,'c') )
         allocate ( b1(2)%d2, source = childdata(4,1)(4,'d') )
   end select

   select type ( b2 )
      type is (child(4,4))
         allocate ( b2(1,1)%d1, source = childdata(4,1)(5,'e') )
         allocate ( b2(1,1)%d2, source = childdata(4,1)(6,'f') )
         allocate ( b2(2,1)%d1, source = childdata(4,1)(7,'g') )
         allocate ( b2(2,1)%d2, source = childdata(4,1)(8,'h') )
         allocate ( b2(1,2)%d1, source = childdata(4,1)(9,'i') )
         allocate ( b2(1,2)%d2, source = childdata(4,1)(10,'j') )
         allocate ( b2(2,2)%d1, source = childdata(4,1)(11,'k') )
         allocate ( b2(2,2)%d2, source = childdata(4,1)(12,'l') )
   end select

   allocate ( b3(1,1)%d1, source = childdata(4,1)(13,'m') )
   allocate ( b3(1,1)%d2, source = childdata(4,1)(14,'n') )
   allocate ( b3(2,1)%d1, source = childdata(4,1)(15,'o') )
   allocate ( b3(2,1)%d2, source = childdata(4,1)(16,'p') )
   allocate ( b3(1,2)%d1, source = childdata(4,1)(17,'q') )
   allocate ( b3(1,2)%d2, source = childdata(4,1)(18,'r') )
   allocate ( b3(2,2)%d1, source = childdata(4,1)(19,'s') )
   allocate ( b3(2,2)%d2, source = childdata(4,1)(20,'t') )
   allocate ( b4(2,2), source = b2 )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, data, childdata

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
      type is (child(4,4))
         select type ( d1 => m%d1 )
            type is (data(4))
               error stop 6_4
            type is (childdata(4,*))
               write (unit, *, iostat=iostat )      d1%i, d1%c
         end select
         select type ( d2 => m%d2 )
            type is (data(4))
               error stop 6_4
            type is (childdata(4,*))
               write (unit, *, iostat=iostat )      d2%i, d2%c
         end select
   end select


   iomsg = 'dtiowrite'

end subroutine
