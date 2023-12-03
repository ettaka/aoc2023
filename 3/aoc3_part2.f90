program aoc3_part1
  implicit none
  character(1) :: input(140, 140)
  integer :: mask(140, 140)
  integer :: stat, answer
  integer, allocatable :: numbers(:)
  integer :: uniques(140, 2)

  answer = 0

  block
    integer :: i, j
    character(140) :: line
    open(10, file='input.txt', status='old', iostat=stat)
    do i=1,140
        read(10, '(A)') line
        do j=1,140
          input(i,j) = line(j:j)
        end do
    end do
    close(10)
  end block

  where (is_number(input))
    mask = 1
  else where (input=='*')
    mask = -1
  else where
    mask = 0
  end where

  where (mask(1:139, 1:139) == -1 .and. mask(2:140, 2:140) == 1)
    mask(2:140, 2:140) = 2
  end where
  where (mask(2:140, 2:140) == -1 .and. mask(1:139, 1:139) == 1)
    mask(1:139, 1:139) = 2
  end where
  where (mask(2:140, 1:139) == -1 .and. mask(1:139, 2:140) == 1)
    mask(1:139, 2:140) = 2
  end where
  where (mask(1:139, 2:140) == -1 .and. mask(2:140, 1:139) == 1)
    mask(2:140, 1:139) = 2
  end where

  where (mask(1:139, 1:140) == -1 .and. mask(2:140, 1:140) == 1)
    mask(2:140, 1:140) = 2
  end where
  where (mask(1:140, 1:139) == -1 .and. mask(1:140, 2:140) == 1)
    mask(1:140, 2:140) = 2
  end where
  where (mask(2:140, 1:140) == -1 .and. mask(1:139, 1:140) == 1)
    mask(1:139, 1:140) = 2
  end where
  where (mask(1:140, 2:140) == -1 .and. mask(1:140, 1:139) == 1)
    mask(1:140, 1:139) = 2
  end where

  block
    integer :: i, j
    logical :: flood

    flood = .false.

    do i=1,140
      do j=1,140

        if (mask(i,j) /= 1) then
          flood = mask(i,j) == 2
        end if

        if (flood) mask(i,j) = 2
      end do
      do j=140,1,-1

        if (mask(i,j) /= 1) then
          flood = mask(i,j) == 2
        end if

        if (flood) mask(i,j) = 2
      end do
    end do
  end block

  block
    integer :: i, j, m_number, n, k
    logical :: m_start, visited
    character(10) :: nchar

    allocate(numbers(0))

    nchar = '          '
    m_start = .false.
    m_number = 1
    visited = .false.
    k=1
    do i=1,140
      do j=1,140
        if (mask(i,j) == 2) then
          visited = .true.
          if (.not. m_start) m_number = m_number+1
          mask(i,j)=m_number
          nchar(k:k) = input(i,j)
          m_start = .true.
          k=k+1
        else
          if (visited) then
            read(nchar, '(I10)') n
            if (n /= 0) then
              numbers = [numbers, [n]]
            end if
          end if
          nchar = '          '
          k=1
          m_start = .false.
        end if
      end do
    end do
  end block

  block
    integer :: i, j, k
    integer :: na(8)
    integer, allocatable :: unique_numbers(:)

    do i=1,140
      do j=1,140
        if (mask(i,j) == -1) then
          na = numbers_around(mask, i, j)
          where (na < 0)
            na = 0
          end where
          unique_numbers = get_unique_numbers(na)

          if (count(unique_numbers>0) == 2) then 
            print *, "unique_numbers", unique_numbers
            print *, "unique_number1", numbers(unique_numbers(1)-1)
            print *, "unique_number2", numbers(unique_numbers(2)-1)
            answer = answer + numbers(unique_numbers(1)-1)*numbers(unique_numbers(2)-1)
          end if
        end if
      end do
    end do
  end block

  print *, input(1,:)
  print *, input(2,:)
  print *, input(3,:)
  print *, input(4,:)

  write(*, '(140I1)') mask(1,:)
  write(*, '(140I1)') mask(2,:)
  write(*, '(140I1)') mask(3,:)
  write(*, '(140I1)') mask(4,:)

  print *, answer

  contains
    function get_unique_numbers(x) result(z)
      implicit none
      integer, intent(in) :: x(:)
      integer, allocatable :: y(:), z(:)

      allocate(z(0))
      y = x(:)
      do while (size(y) > 0)
          z = [z, [y(1)]]
          y = pack(y,mask=(y(:) /= y(1)))
      end do
      z = pack(z, mask=(z(:) /= 0))
    end function get_unique_numbers

    logical elemental function is_number(ch)
      character(1), intent(in) :: ch
      is_number = ch .le. '9' .and. ch .ge. '0'
    end function

    function numbers_around(mask, i, j)
      integer, intent(in) :: mask(140, 140)
      integer, intent(in) :: i, j
      integer :: numbers_around(8), perm1(8), perm2(8)
      integer :: k, l

      numbers_around = 0
      perm1 = [i+1,i+1,i  ,i-1,i-1,i-1,i  ,i+1]
      perm2 = [j  ,j+1,j+1,j+1,j  ,j-1,j-1,j-1]
      do k = 1, 8
          if (perm1(k) > 0 .and. perm1(k) < 141 .and. perm2(k) > 0 .and. perm2(k) < 141) then
            numbers_around(k) = mask(perm1(k),perm2(k))
          else
            numbers_around(k) = 0
          end if
      end do
    end function

end program aoc3_part1
