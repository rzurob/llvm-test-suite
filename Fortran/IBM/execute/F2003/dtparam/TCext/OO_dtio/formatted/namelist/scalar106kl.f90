! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar106kl
!*
!*  DATE                       : 2007-07-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with scalar component with explicit shape array component (Input)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n
   type :: mydata (kmd)
      integer, kind :: kmd
      integer(kmd) ::  i = -9999
   end type
end module

module m
   use n
   type :: base (kb,lb1,lb2)
      integer, kind :: kb
      integer, len :: lb1,lb2
      type(mydata(kb)) :: b(lb1) = mydata(kb)() ! was "(/ mydata(), mydata() /)", but this burns in len=2; avoid AC this way
      character(lb2) :: c = 'xxx'
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*,*)), intent(inout) :: dtv ! tcx: (4,*,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
end module

program scalar106kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:,:)), allocatable :: b1 ! tcx: (4,:,:)
   class(base(4,:,:)), pointer     :: b2 ! tcx: (4,:,:)
   type(base(4,2,3))               :: b3 ! tcx: (4,2,3)
   class(base(4,:,:)), pointer     :: b4 ! tcx: (4,:,:)

   namelist /nml1/ b1
   namelist /nml2/ b2
   namelist /nml3/ b3
   namelist /nml3/ b4

   open (1, file = 'scalar106kl.1', form='formatted', access='stream' )

   allocate(base(4,2,3):: b1, b2, b4)

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( ( b1%c /= 'abc' ) .or. ( b1%b(1)%i /= 333 ) .or. ( b1%b(2)%i /= 444 ) )   error stop 4_4
   if ( ( b2%c /= 'def' ) .or. ( b2%b(1)%i /= 444 ) .or. ( b2%b(2)%i /= 555 ) )   error stop 5_4
   if ( ( b3%c /= 'ghi' ) .or. ( b3%b(1)%i /= 999 ) .or. ( b3%b(2)%i /= 100 ) )   error stop 6_4
   if ( ( b4%c /= 'jkl' ) .or. ( b4%b(1)%i /= 989 ) .or. ( b4%b(2)%i /= 878 ) )   error stop 7_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, mydata

   interface read(formatted)
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import mydata
         class(mydata(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4,*,*)), intent(inout) :: dtv ! tcx: (4,*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(mydata(4)) ::  b(2) ! tcx: (4)

   namelist /nml/ b

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   read (unit, "(A3)", iostat=iostat )        dtv%c

   read (unit, nml, iostat=iostat, iomsg = iomsg )

   dtv%b=b

   if ( ( iomsg /= 'dtioread1' ) .or. ( iostat /= 0 ) ) error stop 10_4

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: mydata

   class(mydata(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 11_4
   if ( size(v_list, 1) /= 0 ) error stop 12_4

   read (unit, "(I3)", iostat=iostat )        dtv%i

   iomsg = 'dtioread1'

end subroutine


! Extensions to introduce derived type parameters:
! type: mydata - added parameters (kmd) to invoke with (4) / declare with (4) - 6 changes
! type: base - added parameters (kb,lb1,lb2) to invoke with (4,2,3) / declare with (4,*,*) - 6 changes
