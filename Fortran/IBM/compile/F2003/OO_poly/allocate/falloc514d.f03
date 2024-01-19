! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/11/2005
!*
!*  DESCRIPTION                : ALLOCATE (specifical cases where source-expr
!                               are function reference of null())
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc514d
    integer, pointer :: u
    class (*), allocatable :: x(:)

    allocate (u, source=null())         !<-- should diagnose

    allocate (x(2), source=null(u))     !<-- should diagnose
end
