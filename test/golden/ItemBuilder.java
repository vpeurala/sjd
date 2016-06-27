import java.util.Objects;

public class ItemBuilder {
    private String uid;
    private long itemId;

    public static ItemBuilder create() {
        return new ItemBuilder();
    }

    public static ItemBuilder from(Item item) {
        return new ItemBuilder().
            setUid(item.getUid()).
            setItemId(item.getItemId());
    }

    public ItemBuilder setUid(String uid) {
        Objects.requireNonNull(uid);
        this.uid = uid;
        return this;
    }

    public ItemBuilder setItemId(long itemId) {
        Objects.requireNonNull(itemId);
        this.itemId = itemId;
        return this;
    }

    public Item build() {
        return new Item(
            this.uid,
            this.itemId
        );
    }
}