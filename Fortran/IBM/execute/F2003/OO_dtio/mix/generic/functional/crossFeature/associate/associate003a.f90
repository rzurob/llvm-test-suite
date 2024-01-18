!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Associate Construct
!*                                    -  selector is a function return with unformatted i/o

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
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted) => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      function returnmyself(dtv)
         class(base), intent(in) :: dtv
         class(base), allocatable :: returnmyself
         allocate ( returnmyself, source = dtv )
      end function

end module

program associate003a
   use m

   type(base) :: b1(3)   = (/ base('abc'), base('def'), base('ghi') /)
   class(base), allocatable :: b2(:)
   type(child) :: c1(2,2)

   type(base)  :: b11, b12, b13(3)
   type(child) :: c11, c12(4)

   integer :: stat
   character(200) :: msg

   allocate ( b2(3), source = (/ base('ABC'), base('DEF'), base('GHI') /) )

   c1 = reshape ( source = (/ child('ABC', 1001), child('def', 1002), child('GHI', 1003), child('jkl', 1004)/), shape = (/2,2/) )

   open ( 1, file = 'associate003a.1', form='unformatted', access='sequential' )


   associate ( a => b1(1)%returnmyself(), b => b2(1)%returnmyself(), c => c1(1,1)%returnmyself() )
      write ( 1, iostat = stat, iomsg = msg )    a
      write ( 1, iostat = stat, iomsg = msg )    b
      write ( 1, iostat = stat, iomsg = msg )    c
   end associate

   associate ( d => returnbase(b1), e => returnchild(c1) )
      write ( 1, iostat = stat, iomsg = msg ) d
      write ( 1, iostat = stat, iomsg = msg ) e
   end associate

   rewind 1

   read (1, iostat = stat, iomsg = msg ) b11
   read (1, iostat = stat, iomsg = msg ) b12
   read (1, iostat = stat, iomsg = msg ) c11
   read (1, iostat = stat, iomsg = msg ) b13
   read (1, iostat = stat, iomsg = msg ) c12

   if ( ( b11%c /= 'abc') .or. ( b12%c /= 'ABC' ) .or. ( c11%c /= 'ABC' ) .or. ( c11%i /= 1001 ) ) error stop 1_4
   if ( ( b13(1)%c /= 'abc' ) .or. ( b13(2)%c /= 'def' ) .or. ( b13(3)%c /= 'ghi' ) )              error stop 2_4
   if ( ( c12(1)%c /= 'ABC' ) .or. ( c12(2)%c /= 'def' ) .or. ( c12(3)%c /= 'GHI' ) .or. ( c12(4)%c /= 'jkl' ) .or. &
        ( c12(1)%i /= 1001 ) .or. ( c12(2)%i /= 1002 ) .or. ( c12(3)%i /= 1003 ) .or. ( c12(4)%i /= 1004 ))   error stop 3_4

   close ( 1, status ='delete')

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

