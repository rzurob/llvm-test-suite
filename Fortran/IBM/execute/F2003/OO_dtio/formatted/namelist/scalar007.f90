! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with class hierarchy (Output)
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
      character(3) :: a1
   end type

   type, extends(A) :: B
      integer(4) :: b1
   end type

   type, extends(B) :: C
      logical, pointer    :: c1 => null()
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import A
         class(A), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar007
   use m

   integer :: stat
   character(200) :: msg = ''

   class(A), allocatable :: b1
   class(A), pointer     :: b2
   logical, target :: g = .true.

   namelist /nml1/ b1
   namelist /nml1/ b2
   namelist /nml2/ b2

   open (1, file = 'scalar007.1', form='formatted', access='sequential' )

   allocate ( b1, source = A('ibm') )
   allocate ( b2, source = B(a1='ibm',b1=888) )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   allocate ( b2, source = C('ibm',888, g ) )
   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: A, B, C

   class(A), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type (dtv)
      type is (A)
         write (unit, "('a= ',A3,1X)", iostat=iostat )                                   dtv%a1
      type is (B)
         write (unit, "('a=',A3,1X, 'b=', I4, 1X)", iostat=iostat )                      dtv%a1, dtv%b1
      type is (C)
         write (unit, "('a=',A3,1X, 'b=', I4, 1X, 'c=', L4, 1X)", iostat=iostat )        dtv%a1, dtv%b1, dtv%c1
   end select

   iomsg = 'dtiowrite'

end subroutine
