! GM DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/abstracti/functional/abstracti005.f

!************************************************************************
!* ======================================================================
!*
!*  TEST CASE NAME             : abstracti005kl_dlp_rb
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-05 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*  Try namelist formatting implicit array objects and define implicit statement inside DTIO (input)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: abstractdata(k1,n1) ! (k1=4,n1=20)
       integer, kind :: k1
       integer, len  :: n1
   contains
      procedure(inf),deferred, pass :: get
   end type

   type, extends(abstractdata) :: data
      integer(k1) :: i = -999
   contains
      procedure, pass :: get
   end type

   type :: base(n2,k2) ! (n2=3,k2=4)
      integer, kind                          :: k2
      integer, len                           :: n2
      character(n2)                          :: c = 'xxx'
      class(abstractdata(k2,:)), allocatable :: d
   end type

   interface
      integer function inf (dtv)
         import abstractdata
         class(abstractdata(4,*)), intent(in) :: dtv ! tcx: (4,*)
      end function
   end interface

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import abstractdata
         class(abstractdata(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   integer function get (dtv)
      class(data(4,*)), intent(in) :: dtv ! tcx: (4,*)
      get = dtv%i
   end function

end module

program abstracti005kl_dlp_rb
   use m
   implicit type(base(3,4))   (A-M) ! tcx: (3,4)
   implicit class(base(3,4))  (N-Z) ! tcx: (3,4)

   namelist /nml1/ b1, b2
   namelist /nml2/ z3, z4

   dimension :: b1(2)
   allocatable  :: b2(:)
   pointer  :: z3(:)
   allocatable :: z4(:)

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'abstracti005kl_dlp_rb.1', form='formatted', access='sequential' )

   b1 =  (/ base(3,4)(d=data(4,20)()), base(3,4)(d=data(4,20)()) /) ! tcx: (3,4) ! tcx: (4,20)
   allocate ( b2(3), source = (/ base(3,4)(d=data(4,20)()), base(3,4)(d=data(4,20)()), base(3,4)(d=data(4,20)()) /) ) ! tcx: (4,20)
   allocate ( z3(3), source = b2 )
   allocate ( z4(2), source = b1 )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   select type ( d1 => b1(1)%d )
      type is (data(4,*)) ! tcx: (4,*)
         select type ( d2 => b1(2)%d )
            type is (data(4,*)) ! tcx: (4,*)
               if ( ( b1(1)%c /= 'b11' ) .or. ( d1%i /= 1001 ) .or. ( b1(2)%c /= 'b12' ) .or. ( d2%i /= 1002 ))   error stop 2_4
            class default
               error stop 3_4
         end select
      class default
         error stop 4_4
   end select

   select type ( d1 => b2(1)%d )
      type is (data(4,*)) ! tcx: (4,*)
         select type ( d2 => b2(2)%d )
            type is (data(4,*)) ! tcx: (4,*)
               select type ( d3 => b2(3)%d )
                  type is (data(4,*)) ! tcx: (4,*)
                     if ( ( b2(1)%c /= 'b21' ) .or. ( d1%i /= 2001 ) .or. ( b2(2)%c /= 'b22' ) .or. ( d2%i /= 2002 )  .or. &
                          ( b2(3)%c /= 'b23' ) .or. ( d3%i /= 2003 ) )   error stop 5_4
                  class default
                     error stop 6_4
               end select
            class default
               error stop 7_4
         end select
      class default
         error stop 8_4
   end select

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 9_4


   select type ( d1 => z3(1)%d )
      type is (data(4,*)) ! tcx: (4,*)
         select type ( d2 => z3(2)%d )
            type is (data(4,*)) ! tcx: (4,*)
               select type ( d3 => z3(3)%d )
                  type is (data(4,*)) ! tcx: (4,*)
                     if ( ( z3(1)%c /= 'z31' ) .or. ( d1%i /= 3001 ) .or. ( z3(2)%c /= 'z32' ) .or. ( d2%i /= 3002 )  .or. &
                          ( z3(3)%c /= 'z33' ) .or. ( d3%i /= 3003 ) )   error stop 10_4
                  class default
                     error stop 11_4
               end select
            class default
               error stop 12_4
         end select
      class default
         error stop 13_4
   end select

   select type ( d1 => z4(1)%d )
      type is (data(4,*)) ! tcx: (4,*)
         select type ( d2 => z4(2)%d )
            type is (data(4,*)) ! tcx: (4,*)
               if ( ( z4(1)%c /= 'z41' ) .or. ( d1%i /= 4001 ) .or. ( z4(2)%c /= 'z42' ) .or. ( d2%i /= 4002 ))   error stop 14_4
            class default
               error stop 15_4
         end select
      class default
         error stop 16_4
   end select

end program abstracti005kl_dlp_rb

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, abstractdata, read(formatted), readformatteddata

   implicit class(abstractdata(4,20)) (X) ! tcx: (4,20)

   class(base(*,4)), intent(inout) :: dtv ! tcx: (*,4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   namelist /dtio/ x1
   allocatable :: x1

   if ( iotype /= "NAMELIST" ) error stop 17_4
   if ( size(v_list, 1) /= 0 ) error stop 18_4

   read (unit, *, iostat=iostat )        dtv%c

   if ( iostat /= 0 ) error stop 19_4

   allocate ( x1, source = dtv%d )
   read (unit, dtio, iostat=iostat, iomsg = iomsg )

   if ( allocated ( dtv%d ) ) deallocate ( dtv%d )

   allocate ( dtv%d , source = x1 )

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: abstractdata, data

   class(abstractdata(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 20_4
   if ( size(v_list, 1) /= 0 ) error stop 21_4

   select type ( dtv )
      class is (abstractdata(4,*)) ! tcx: (4,*)
         error stop 22_4
      type is (data(4,*)) ! tcx: (4,*)
         read (unit, *, iostat=iostat )        dtv%i
   end select

end subroutine
