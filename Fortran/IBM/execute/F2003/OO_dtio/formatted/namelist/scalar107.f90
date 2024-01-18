! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with class hierarchy (Input)
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

   type :: A
      character(3) :: a1 = 'nil'
   end type

   type, extends(A) :: B
      integer(4) :: b1 = -9999
   end type

   type, extends(B) :: C
      logical    :: c1 = .false.
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import A
         class(A), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar107
   use m

   integer :: stat
   character(200) :: msg = ''

   class(A), allocatable :: b1
   class(A), pointer     :: b2
   class(A), allocatable :: b3
   class(B), pointer     :: b4

   namelist /nml1/ b1
   namelist /nml1/ b2
   namelist /nml1/ b3
   namelist /nml1/ b4

   open (1, file = 'scalar107.1', form='formatted', access='sequential' )

   allocate ( b1, source = A() )
   allocate ( b2, source = B() )
   allocate ( b3, source = C() )
   allocate ( b4, source = C() )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   ! check if values are read correctly

   select type (b1)
      type is (A)
         if ( b1%a1 /= 'ibm' ) error stop 2_4
      class default
         error stop 3_4
   end select

   select type (b2)
      type is (B)
         if ( ( b2%a1 /= 'ftn' ) .or. ( b2%b1 /= 1234 ) ) error stop 4_4
      class default
         error stop 5_4
   end select

   select type (b3)
      type is (C)
         if ( ( b3%a1 /= 'IBM' ) .or. ( b3%b1 /= 2345 ) .or. ( b3%c1 .neqv. .true. ) ) error stop 6_4
      class default
         error stop 7_4
   end select

   select type (b4)
      type is (C)
         if ( ( b4%a1 /= 'FTN' ) .or. ( b4%b1 /= 3456 ) .or. ( b4%c1 .neqv. .false. ) ) error stop 8_4
      class default
         error stop 9_4
   end select

   deallocate (b3)

   allocate ( b2, source = C() )
   allocate ( b3, source = A() )
   allocate ( b4, source = C() )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 10_4

   select type (b1)
      type is (A)
         if ( b1%a1 /= 'ibm' ) error stop 11_4  !<- no change
      class default
         error stop 12_4
   end select

   select type (b2)
      type is (C)
         if ( ( b2%a1 /= 'FTN' ) .or. ( b2%b1 /= 4321 ) .or. ( b2%c1 .neqv. .true. ) ) error stop 13_4
      class default
         error stop 14_4
   end select

   select type (b3)
      type is (A)
         if ( b3%a1 /= 'IBM' ) error stop 15_4
      class default
         error stop 16_4
   end select

   select type (b4)
      type is (C)
         if ( ( b4%a1 /= 'IBM' ) .or. ( b4%b1 /= 6789 ) .or. ( b4%c1 .neqv. .true. ) ) error stop 17_4
      class default
         error stop 18_4
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: A, B, C

   class(A), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 19_4
   if ( size(v_list, 1) /= 0 ) error stop 20_4

   select type (dtv)
      type is (A)
         read (unit, "(A3)", iostat=iostat )                                     dtv%a1
      type is (B)
         read (unit, "(A3,1X, I4)", iostat=iostat )                      dtv%a1, dtv%b1
      type is (C)
         read (unit, "(A3,1X, I4, 1X, L4)", iostat=iostat )      dtv%a1, dtv%b1, dtv%c1
   end select

   iomsg = 'dtioread'

end subroutine
