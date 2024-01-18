! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar010kl
!*
!*  DATE                       : 2007-07-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic entities contains polymorphic components (Output)
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

   type basedata (kbd,lbd) ! kbd=4,2
      integer, kind :: kbd
      integer, len :: lbd
      integer(kbd) :: i1(lbd) = -9 ! was "(/ -9,-9 /)" - probably not intended to test AC's, so we won't do "[(-9,i=1,lbd)]"
   end type

   type, extends(basedata) :: childdata (kcd,lcd) ! kcd,lcd=4,2
      integer, kind :: kcd
      integer, len :: lcd
      integer(kcd) :: i2(lcd) = -9 ! was "(/ -9,-9 /)" - probably not intended to test AC's, so we won't do "[(-9,i=1,lcd)]"
   end type

   type base (kb) ! kb=4
      integer, kind :: kb
      class(basedata(kb,:)), pointer :: bd ! tcx: (kb,:)
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      class(basedata(kc,:)), allocatable :: cd ! tcx: (kc,:)
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar010kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1 ! tcx: (4)
   class(base(4)), pointer     :: b2 ! tcx: (4)
   namelist /nml/ b1, b2

   open (1, file = 'scalar010kl.1', form='formatted', access='sequential' )
   allocate(b1)
   allocate(basedata(4,2):: b1%bd) ! tcx: basedata(4,2)
   allocate(child(4,4) :: b2 ) ! tcx: (4)

   select type(b2)
      type is (child(4,4)) ! tcx: (4)
         allocate(childdata(4,2,4,2) :: b2%bd, b2%cd) ! tcx: (4,2)
         select type (g => b2%bd)
            type is (childdata(4,*,4,*)) ! tcx: (4,*)
               g%i1 = (/123, 234/)
               g%i2 = (/345, 456/)
         end select
         select type (g=>b2%cd)
            type is (childdata(4,*,4,*)) ! tcx: (4,*)
               g%i1 = (/567, 678/)
               g%i2 = (/789, 890/)
         end select
   end select

   b1%bd%i1= (/ 567, 890 /)


   write (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, basedata

   interface write(formatted)
      subroutine datawriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import basedata
         class(basedata(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   class(basedata(4,:)), allocatable :: dummy1, dummy2 ! tcx: (4,:)

   namelist /basedtio/ dummy1
   namelist /childdtio/ dummy1, dummy2

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (child(4,4)) ! tcx: (4)
         allocate (dummy1, source = dtv%bd)
         allocate (dummy2, source = dtv%cd)
         write (unit, childdtio, iostat = iostat, iomsg = iomsg)
      class default            !<- if it's not type child, it's type base
         allocate (dummy1, source = dtv%bd)
        write (unit, basedtio, iostat = iostat, iomsg = iomsg)
   end select

   if ( iomsg /= 'datadtio' ) error stop 4_4

   iomsg = 'dtiowrite'

end subroutine

subroutine datawriteformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: basedata, childdata

   class(basedata(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (basedata(4,*)) ! tcx: (4,*)
         write (unit, "('basedatacontent:',2(I4) )" , iostat = iostat )   dtv%i1
      type is (childdata(4,*,4,*)) ! tcx: (4,*)
         write (unit, "('basedatacontent:',2(I4),/)" , iostat = iostat )   dtv%i1
         write (unit, "('childdatacontent:',2(I4),/ )", iostat = iostat )   dtv%i2
   end select

   iomsg = 'datadtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: basedata - added parameters (kbd,lbd) to invoke with (4,2) / declare with (4,*) - 6 changes
! type: childdata - added parameters (kcd,lcd) to invoke with (4,2) / declare with (4,*) - 4 changes
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (kc) to invoke with (4) / declare with (4) - 3 changes
