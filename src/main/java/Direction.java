import java.util.function.Function;

public enum Direction {

    UP, DOWN, LEFT, RIGHT;

    static {
        UP.straight = UP;
        UP.left = LEFT;
        UP.right = RIGHT;

        DOWN.straight = DOWN;
        DOWN.left = RIGHT;
        DOWN.right = LEFT;

        LEFT.straight = LEFT;
        LEFT.left = DOWN;
        LEFT.right = UP;

        RIGHT.straight = RIGHT;
        RIGHT.left = UP;
        RIGHT.right = DOWN;
    }

    private Direction straight;
    private Direction left;
    private Direction right;

    public Direction straight() {
        return straight;
    }

    public Direction left() {
        return left;
    }

    public Direction right() {
        return right;
    }

}
