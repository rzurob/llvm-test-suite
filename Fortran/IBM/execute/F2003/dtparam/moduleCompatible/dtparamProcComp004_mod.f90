
module m
    type base (n)
        integer, len :: n

        logical(1) :: mask(n) = .true.
        procedure(negation), pointer :: inverse => null()
    end type

    interface
        function negation (b)
        import
            class(base(*)), intent(in) :: b
            type (base(b%n)) negation
        end function
    end interface
end module
