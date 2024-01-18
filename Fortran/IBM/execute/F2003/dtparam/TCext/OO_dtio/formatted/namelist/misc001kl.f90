! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : misc001kl
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting parent write, and list directed in child
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
   type base (kb,lb1,lb2)
      integer, kind :: kb
      integer, len :: lb1,lb2
      integer(kb) :: i(lb1)
      character(lb2) :: c
   end type
end module

module m1
   use m
   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
end module

program misc001kl
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:,:)), allocatable :: b1 ! tcx: (4,:,:)
   class(base(4,:,:)), pointer     :: b2 ! tcx: (4,:,:)
   type(base(4,2,3))               :: b3 ! tcx: (4,2,3)
   type(base(4,2,3)), allocatable  :: b4 ! tcx: (4,:,:)
   type(base(4,2,3)), pointer      :: b5 ! tcx: (4,:,:)

   namelist /nml1/ b1, b1
   namelist /nml1/ b2, b2
   namelist /nml2/ b3, b1
   namelist /nml2/ b4, b5
   namelist /nml3/ b5, b3

   open (1, file = 'misc001kl.1', form='formatted', access='sequential' )
   allocate(base(4,2,3):: b1, b2, b4, b5) ! tcx: base(4,2,3)

   b1%i = (/1,2/)
   b2%i = (/3,4/)
   b3%i = (/5,6/)
   b4%i = (/7,8/)
   b5%i = (/9,10/)

   b1%c = 'abc'
   b2%c = 'def'
   b3%c = 'ghi'
   b4%c = 'jkl'
   b5%c = 'mno'

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   b1%i = (/11,12/)
   b1%c = 'IBM'

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   b3%i = (/13,14/)
   b3%c = 'FTN'

   write (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, *, iostat=iostat )      dtv%i

!   associate ( d => dtv%c )
      write (unit, *, iostat=iostat )      dtv%c
!   end associate

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb1,lb2) to invoke with (4,2,3) / declare with (4,*,*) - 8 changes
