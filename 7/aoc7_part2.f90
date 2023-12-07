program aoc7_part2
  use iso_fortran_env
  implicit none

  integer(int64), parameter :: n_hands = 1000
  integer(int64) :: stat, i
  integer(int64) :: bid(n_hands), hand(5, n_hands), &
                    types(n_hands), hand_for_type(5, n_hands)
  integer :: ranking(n_hands)
  integer(int64) :: magnitude(n_hands)

  character(5) :: hand_char(n_hands)

  open(10, file='input.txt', status='old', iostat=stat)

  bid = 0
  hand = 0
  hand_for_type = 0
  i = 0
  do while (.true.)
    i = i+1
    read(10, "(A5,X,I5)", end=99) hand_char(i), bid(i)

    block
      integer(int64) :: j, n_jokers, maxind(5)
      integer(int64) :: card_amounts(2:14)

      do j = 1, len(hand_char(i))
        hand(j, i) = map_cardnum_to_number(hand_char(i)(j:j))
      end do

      card_amounts=0
      n_jokers = count(hand(:, i) == 1)
      hand_for_type(:, i) = hand(:, i)
      if (n_jokers>0) then
        do j = 2, 14
          card_amounts(j) = count(hand(:, i)==j)
        end do

!        write(*, '(5I3,X,13I2)') hand(:, i), card_amounts
!        write(*, '(A6,X,I3)') 'maxval', maxval(card_amounts)
        maxind = findloc(card_amounts, maxval(card_amounts), back=.true., dim=1)
!        print *, "max number:", maxind(1)+1
!        write(*, '(5I3,X,13I2)') hand_for_type(:, i), card_amounts
        where (hand(:, i) == 1)
          hand_for_type(:, i) = maxind(1)+1
        else where
          hand_for_type(:, i) = hand(:, i)
        end where
      end if

    end block

    block
      integer(int64), allocatable :: unique_nums(:), type_uniques(:)
      integer(int64), allocatable :: n_uniques(:)
      character(10) :: size_char
      integer(int64) :: j

      unique_nums = get_unique_numbers(hand_for_type(:, i))
      write(size_char, '(I10)') size(unique_nums, dim=1)

      allocate(n_uniques(size(unique_nums, dim=1)))
      n_uniques = 0
      do j = 1, size(unique_nums, dim=1)
        n_uniques(j) = count(hand_for_type(:,i)==unique_nums(j))
      end do

      if (size(n_uniques, dim=1) == 5) then
        types(i) = 1
      else if (size(n_uniques, dim=1) == 4 .and. count(n_uniques==2)==1) then
        types(i) = 2
      else if (size(n_uniques, dim=1) == 3 .and. count(n_uniques==2)==2) then
        types(i) = 3
      else if (size(n_uniques, dim=1) == 3 .and. count(n_uniques==3)==1) then
        types(i) = 4
      else if (size(n_uniques, dim=1) == 2 .and. &
               count(n_uniques==3)==1 .and. &
               count(n_uniques==2)==1) then
        types(i) = 5
      else if (size(n_uniques, dim=1) == 2 .and. &
               count(n_uniques==4)==1 .and. &
               count(n_uniques==1)==1) then
        types(i) = 6
      else if (size(n_uniques, dim=1) == 1) then
        types(i) = 7
      else
        types(i) = 0
      end if

      magnitude(i) = types(i) * 100_int64**7
      do j = 1, 5
        magnitude(i) = magnitude(i) + hand(j, i) * 100_int64**(5-j)
      end do

!      write(*,'(5I3)') hand_for_type(:, i)
!      write(*, '(A5,X,5(I2,X),X,'//trim(size_char)//'(I2,X),X,'//trim(size_char)//'I2,X,I2,X,I15)') &
!        hand_char(i), hand(:, i), unique_nums, n_uniques, types(i), magnitude(i)
    end block
  end do

  99 continue

  do i = 1, n_hands
    ranking(i) = count(mask=magnitude < magnitude(i))+1
  end do

  block
    integer(int64) :: answer
    integer(int64) :: l(5)

  answer = 0
  do i = 1, n_hands
    l = findloc(ranking, i, dim=1)

    write(*, *) hand_char(l(1)), ranking(l(1)), bid(l(1))

    answer = answer + ranking(l(1))*bid(l(1))

  end do
  print *, "answer", answer
  endblock

  print *, "winning hand:", hand(:,findloc(ranking,n_hands))
  print *, "maxval(ranking):", maxval(ranking)
  print *, "minval(ranking):", minval(ranking)

  print *, sum(ranking * bid)


  contains

    function get_unique_numbers(x) result(z)
      implicit none
      integer(int64), intent(in) :: x(:)
      integer(int64), allocatable :: y(:), z(:)

      allocate(z(0))
      y = x(:)
      do while (size(y) > 0)
          z = [z, [y(1)]]
          y = pack(y,mask=(y(:) /= y(1)))
      end do
      z = pack(z, mask=(z(:) /= 0))
    end function get_unique_numbers

    pure integer(int64) function map_cardnum_to_number(cardnum) result (num)
      character(1), intent(in) :: cardnum

      select case (cardnum)
         case ('A') 
           num = 13
         case ('K') 
           num = 12
         case ('Q') 
           num = 11
         case ('T') 
           num = 10
         case ('9') 
           num = 9
         case ('8') 
           num = 8
         case ('7') 
           num = 7
         case ('6') 
           num = 6
         case ('5') 
           num = 5         
         case ('4') 
           num = 4         
        case ('3') 
           num = 3         
        case ('2')
           num = 2
        case ('J')
           num = 1
      end select
    end function

end program aoc7_part2
