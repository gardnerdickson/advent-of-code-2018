public enum Direction {

    UP, DOWN, LEFT, RIGHT;

    static {
        UP.straight = UP;
        UP.left = LEFT;
        UP.right = RIGHT;
        UP.str = "^";

        DOWN.straight = DOWN;
        DOWN.left = RIGHT;
        DOWN.right = LEFT;
        DOWN.str = "v";

        LEFT.straight = LEFT;
        LEFT.left = DOWN;
        LEFT.right = UP;
        LEFT.str = "<";

        RIGHT.straight = RIGHT;
        RIGHT.left = UP;
        RIGHT.right = DOWN;
        RIGHT.str = ">";
    }

    private Direction straight;
    private Direction left;
    private Direction right;
    private String str;

    public Direction straight() {
        return straight;
    }

    public Direction left() {
        return left;
    }

    public Direction right() {
        return right;
    }

    @Override
    public String toString() {
        return str;
    }

}
