!*  ===================================================================
!*
!*  DATE                       : 2007-07-12 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 namelist formatting
!*                                        Try placing the equal sign further down in namelist record
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
   type base (kb) ! kb=4
      integer, kind :: kb
      integer(kb) :: i = 0
   end type
end module

module m1
   use m
   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
end module


program scalar112kl
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1 ! tcx: (4)

   namelist /nml/ b1
   open (1, file = 'scalar112kl.1', form='formatted', access='sequential', status='old', BLANK='NULL' )

   allocate(b1)

   read  (1,NML=nml, iostat=stat, iomsg=msg)
   if ( b1%i /= 10 ) error stop 1_4
   read  (1,NML=nml, iostat=stat, iomsg=msg)
   if ( b1%i /= 10 ) error stop 2_4
   read  (1,NML=nml, iostat=stat, iomsg=msg)
   if ( b1%i /= 10 ) error stop 3_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, "(I2)", iostat=iostat )      dtv%i

   iomsg = 'dtioread'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 3 changes
