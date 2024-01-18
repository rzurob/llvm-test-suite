
! This is a fake omp_lib module

module omp_lib
   
!    use, intrinsic :: omp_lib, only:OMP_LOCK_KIND 
    integer, parameter :: OMP_LOCK_KIND = 4
contains 
    subroutine OMP_SET_NUM_THREADS(nthread)
       integer nthread
       print *,"This is a fake OMP_SET_NUM_THREADS called with num_thread = ", nthread   
    end subroutine OMP_SET_NUM_THREADS

    subroutine OMP_SET_DYNAMIC(lvar)
       logical lvar
       print *,"This is a fake OMP_SET_DYNAMIC called with value = ", lvar
    end subroutine OMP_SET_DYNAMIC

    subroutine OMP_SET_NESTED(lvar)
       logical lvar
       print *,"This is a fake OMP_SET_NESTED called with value = ", lvar
    end subroutine OMP_SET_NESTED

    function OMP_GET_NUM_THREADS()
       integer omp_get_num_threads
       print *,"This is a fake OMP_GET_NUM_THREADS"
       omp_get_num_threads = 911 
    end function OMP_GET_NUM_THREADS

    function OMP_GET_MAX_THREADS()
       integer omp_get_max_threads
       print *,"This is a fake OMP_GET_MAX_THREADS"
       omp_get_max_threads = 747 
    end function OMP_GET_MAX_THREADS

    function OMP_GET_THREAD_NUM()
       integer omp_get_thread_num
       print *,"This is a fake OMP_GET_THREAD_NUM"
       omp_get_thread_num = 777 
    end function OMP_GET_THREAD_NUM
    
    function OMP_GET_NUM_PROCS()
       integer omp_get_num_procs
       print *,"This is a fake OMP_GET_NUM_PROCS"
       omp_get_num_procs = 888 
    end function OMP_GET_NUM_PROCS

    function OMP_IN_PARALLEL()
       logical omp_in_parallel
       print *,"This is a fake OMP_IN_PARALLEL"
       omp_in_parallel = .true. 
    end function OMP_IN_PARALLEL

    function OMP_GET_DYNAMIC()
       logical omp_get_dynamic
       print *,"This is a fake OMP_GET_DYNAMIC"
       omp_get_dynamic = .false. 
    end function OMP_GET_DYNAMIC
    
    function OMP_GET_NESTED()
       logical omp_get_nested
       print *,"This is a fake OMP_GET_NESTED"
       omp_get_nested = .true. 
    end function OMP_GET_NESTED
    
    subroutine OMP_INIT_LOCK(svar)
       integer(kind=OMP_LOCK_KIND) svar
       print *, "This is a fake OMP_INIT_LOCK called with value = ", svar
    end subroutine OMP_INIT_LOCK
    
    subroutine OMP_SET_LOCK(svar)
       integer(kind=OMP_LOCK_KIND) svar
       print *, "This is a fake OMP_SET_LOCK called with value = ", svar
    end subroutine OMP_SET_LOCK

    subroutine OMP_UNSET_LOCK(svar)
       integer(kind=OMP_LOCK_KIND) svar
       print *, "This is a fake OMP_UNSET_LOCK called with value = ", svar
    end subroutine OMP_UNSET_LOCK

    subroutine OMP_DESTROY_LOCK(svar)
       integer(kind=OMP_LOCK_KIND) svar
       print *, "This is a fake OMP_DESTROY_LOCK called with value = ", svar
    end subroutine OMP_DESTROY_LOCK

    function OMP_TEST_LOCK(svar)
       integer(kind=OMP_LOCK_KIND) svar
       logical omp_test_lock 
       print *, "This is a fake OMP_TEST_LOCK called with value = ", svar
       omp_test_lock = .false.
    end function OMP_TEST_LOCK

    subroutine OMP_INIT_NEST_LOCK(nvar)
       integer*8 nvar
       print *, "This is a fake OMP_INIT_NEST_LOCK called with value = ", nvar
    end subroutine OMP_INIT_NEST_LOCK

    subroutine OMP_SET_NEST_LOCK(nvar)
       integer*8 nvar
       print *, "This is a fake OMP_SET_NEST_LOCK called with value = ", nvar
    end subroutine OMP_SET_NEST_LOCK

    subroutine OMP_UNSET_NEST_LOCK(nvar)
       integer*8 nvar
       print *, "This is a fake OMP_UNSET_NEST_LOCK called with value = ", nvar
    end subroutine OMP_UNSET_NEST_LOCK

    subroutine OMP_DESTROY_NEST_LOCK(nvar)
       integer*8 nvar
       print *, "This is a fake OMP_DESTROY_NEST_LOCK called with value = ", nvar
    end subroutine OMP_DESTROY_NEST_LOCK

    function OMP_TEST_NEST_LOCK(nvar)
       integer*8 nvar
       integer OMP_TEST_NEST_LOCK
       print *, "This is a fake OMP_TEST_NEST_LOCK called with value = ", nvar
       OMP_TEST_NEST_LOCK = 811
    end function OMP_TEST_NEST_LOCK

end module   
