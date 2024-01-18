!**********************************************************************
!*  ===================================================================
!*
!*                               ISO_C_BINDING module.
!*
!*  DATE                       : May 29, 2003
!*
!*  DESCRIPTION                :
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YYYY:  Init:  Comments:
!*  05/29/2003   RJ     -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

! The next macro definition indicates the name of the program.
#define PROGRAM_NAME icdp31123

! If the next macro definition is POINTER_TARGET, then the
! target of the C_PTR object is a pointer.  If the next macro
! definition is ALLOC_TARGET, the target of the C_PTR object
! is an allocatable target.  If the next macro definition is
! PLAIN_TARGET, then the target of the C_PTR object has the
! target attribute.
#define POINTER_TARGET

! If the next macro definition is WHOLE_ARRAY_TARGET, then
! the argument to C_LOC is a whole array.  If the next macro
! definition is CONTIG_ARRAY_SECTION_TARGET, then the
! argument to C_LOC is a contiguous array section.  If the
! next macro definition is NONCONTIG_ARRAY_SECTION_TARGET,
! then the argument to C_LOC is a non-contiguous array
! section.  If the next macro definition is
! ARRAY_ELEMENT_TARGET, then the argument to C_LOC is an
! array element.  If the next macro definition is
! SCALAR_TARGET, then the argument to C_LOC is a scalar,
! named variable.
#define ARRAY_ELEMENT_TARGET

! If the next macro definition is C_PTR_ARRAY, then the
! variable of type C_PTR is an array.  If the next macro
! definition is C_PTR_SCALAR, then the variable of type
! C_PTR is a scalar.
#define C_PTR_ARRAY

! If the next macro definition is C_PTR_STATIC_INIT, then
! the C_PTR variable has the save attribute and is
! statically initialized to null.  If the next macro
! definition is C_PTR_NO_STATIC_INIT, then the C_PTR
! variable does not have the save attribute and is not
! statically initialized.
#define C_PTR_STATIC_INIT

! If the next macro definition is VAR_SHAPE_ARG, then the
! SHAPE argument to C_F_POINTER (if it exists) is a
! variable.  If the next macro definition is
! PARAM_SHAPE_ARG, then the SHAPE argument to C_F_POINTER
! (if it exists) has the parameter attribute.  If the
! next macro definition is AC_SHAPE_ARG, then the SHAPE
! argument to C_F_POINTER (if it exists) is an array
! constructor.
#define VAR_SHAPE_ARG

#include "icdptr.ft"
