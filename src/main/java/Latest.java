import java.io.IOException;

public class Latest {
    public static void main(String[] args) throws IOException {
        new Runner(new MyStrategy(), 31002).run();
    }
}
