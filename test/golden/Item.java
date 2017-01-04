import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Item {
    private final String uid;
    private final long itemId;

    @JsonCreator
    public Item(@JsonProperty("uid") String uid,
                @JsonProperty("itemId") long itemId) {
        Objects.requireNonNull(uid, "Property 'uid' cannot be null.");
        Objects.requireNonNull(itemId, "Property 'itemId' cannot be null.");
        this.uid = uid;
        this.itemId = itemId;
    }

    public String getUid() {
        return uid;
    }

    public long getItemId() {
        return itemId;
    }

    @Override
    public String toString() {
        return "Item@" + System.identityHashCode(this) + ": {"
            + "uid = '" + getUid() + "'"
            + "itemId = '" + getItemId() + "'"
        ;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        if (this.getClass() != o.getClass()) return false;
        Item that = (Item) o;
        if (!this.getUid().equals(that.getUid())) return false;
        if (this.getItemId() != that.getItemId()) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 0;
        result = 31 * result + getUid().hashCode();
        result = 31 * result + Long.hashCode(this.getItemId());
        return result;
    }
}