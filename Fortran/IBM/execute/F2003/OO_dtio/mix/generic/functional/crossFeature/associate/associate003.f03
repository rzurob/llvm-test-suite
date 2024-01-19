!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Associate Construct
!*                                    -  selector is a function return with formatted i/o

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

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: returnmyself
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      function returnmyself(dtv)
         class(base), intent(in) :: dtv
         class(base), allocatable :: returnmyself

         allocate ( returnmyself, source = dtv )
      end function

end module

program associate003
   use m

   type(base) :: b1(3)   = (/ base('abc'), base('def'), base('ghi') /)
   class(base), allocatable :: b2(:)
   type(child) :: c1(2,2)

   integer :: stat
   character(200) :: msg

   allocate ( b2(3), source = (/ base('ABC'), base('DEF'), base('GHI') /) )

   c1 = reshape ( source = (/ child('ABC', 1001), child('def', 1002), child('GHI', 1003), child('jkl', 1004)/), shape = (/2,2/) )

   open ( 1, file = 'associate003.1', form='formatted', access='sequential' )


   associate ( a => b1(1)%returnmyself(), b => b2(1)%returnmyself(), c => c1(1,1)%returnmyself() )
      write ( 1, "(DT)", iostat = stat, iomsg = msg )    a
      write ( 1, "(DT)", iostat = stat, iomsg = msg )    b
      write ( 1, "(DT)", iostat = stat, iomsg = msg )    c
   end associate

   associate ( d => returnbase(b1), e => returnchild(c1) )
      write ( 1, "(3(DT))", iostat = stat, iomsg = msg ) d
      write ( 1, "(4(DT))", iostat = stat, iomsg = msg ) e
   end associate

   contains

      elemental function returnbase(dtv)
         type(base), intent(in) :: dtv
         type(base) :: returnbase
         returnbase = dtv
      end function

      elemental function returnchild(dtv)
         type(child), intent(in) :: dtv
         type(child) :: returnchild
         returnchild = dtv
      end function

end program

