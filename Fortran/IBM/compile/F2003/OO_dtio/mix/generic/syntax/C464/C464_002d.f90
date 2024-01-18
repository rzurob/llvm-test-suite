!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C464 If generic-spec is dtio-generic-spec, the interface of each binding
!*                                                  shall be as specified in 9.5.3.7. The type of the dtv argument
!*                                                  shall be type-name.
!*
!*                                             - dtio procedure define to be a pure subroutine
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
      character(3) :: c
      contains
         procedure, pass :: read => readbase
         generic :: read(formatted) => read
   end type

   contains

      pure subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

      end subroutine

end module

program C464_002d
end program
