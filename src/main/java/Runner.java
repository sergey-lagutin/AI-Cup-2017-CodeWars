import model.Game;
import model.Move;
import model.Player;
import model.PlayerContext;

import java.io.IOException;

public final class Runner {
    private static final String HOST = "127.0.0.1";
    private final RemoteProcessClient remoteProcessClient;
    private final Strategy strategy;

    public Runner(Strategy strategy, int port) throws IOException {
        remoteProcessClient = new RemoteProcessClient(HOST, port);
        this.strategy = strategy;
    }

    @SuppressWarnings("WeakerAccess")
    void run() throws IOException {
        try {
            remoteProcessClient.writeTokenMessage("0000000000000000");
            remoteProcessClient.writeProtocolVersionMessage();
            remoteProcessClient.readTeamSizeMessage();
            Game game = remoteProcessClient.readGameContextMessage();

            PlayerContext playerContext;

            while ((playerContext = remoteProcessClient.readPlayerContextMessage()) != null) {
                Player player = playerContext.getPlayer();
                if (player == null) {
                    break;
                }

                Move move = new Move();
                strategy.move(player, playerContext.getWorld(), game, move);

                remoteProcessClient.writeMoveMessage(move);
            }
        } finally {
            remoteProcessClient.close();
        }
    }
}
