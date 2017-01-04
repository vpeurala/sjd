import java.util.List;
import java.util.Optional;
import java.math.BigDecimal;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class SubSpecials extends Specials {
    private final BigDecimal value;

    @JsonCreator
    public SubSpecials(@JsonProperty("uid") String uid,
                       @JsonProperty("itemId") long itemId,
                       @JsonProperty("validSubProducts") List<String> validSubProducts,
                       @JsonProperty("price") long price,
                       @JsonProperty("showView") boolean showView,
                       @JsonProperty("description") String description,
                       @JsonProperty("change") Optional<String> change,
                       @JsonProperty("buyers") List<Customer> buyers,
                       @JsonProperty("booleanField") boolean booleanField,
                       @JsonProperty("byteField") byte byteField,
                       @JsonProperty("charField") char charField,
                       @JsonProperty("shortField") short shortField,
                       @JsonProperty("intField") int intField,
                       @JsonProperty("longField") long longField,
                       @JsonProperty("floatField") float floatField,
                       @JsonProperty("doubleField") double doubleField,
                       @JsonProperty("stringField") String stringField,
                       @JsonProperty("listField") List<String> listField,
                       @JsonProperty("optionalField") Optional<String> optionalField,
                       @JsonProperty("objectField") Object objectField,
                       @JsonProperty("value") BigDecimal value) {
        super(uid, itemId, validSubProducts, price, showView, description, change, buyers, booleanField, byteField, charField, shortField, intField, longField, floatField, doubleField, stringField, listField, optionalField, objectField);
        Objects.requireNonNull(value, "Property 'value' cannot be null.");
        this.value = value;
    }

    public BigDecimal getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "SubSpecials@" + System.identityHashCode(this) + ": {"
            + "uid = '" + getUid() + "'"
            + "itemId = '" + getItemId() + "'"
            + "validSubProducts = '" + getValidSubProducts() + "'"
            + "price = '" + getPrice() + "'"
            + "showView = '" + isShowView() + "'"
            + "description = '" + getDescription() + "'"
            + "change = '" + getChange() + "'"
            + "buyers = '" + getBuyers() + "'"
            + "booleanField = '" + isBooleanField() + "'"
            + "byteField = '" + getByteField() + "'"
            + "charField = '" + getCharField() + "'"
            + "shortField = '" + getShortField() + "'"
            + "intField = '" + getIntField() + "'"
            + "longField = '" + getLongField() + "'"
            + "floatField = '" + getFloatField() + "'"
            + "doubleField = '" + getDoubleField() + "'"
            + "stringField = '" + getStringField() + "'"
            + "listField = '" + getListField() + "'"
            + "optionalField = '" + getOptionalField() + "'"
            + "objectField = '" + getObjectField() + "'"
            + "value = '" + getValue() + "'"
        ;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        if (this.getClass() != o.getClass()) return false;
        SubSpecials that = (SubSpecials) o;
        if (!this.getUid().equals(that.getUid())) return false;
        if (this.getItemId() != that.getItemId()) return false;
        if (!this.getValidSubProducts().equals(that.getValidSubProducts())) return false;
        if (this.getPrice() != that.getPrice()) return false;
        if (this.isShowView() != that.isShowView()) return false;
        if (!this.getDescription().equals(that.getDescription())) return false;
        if (!this.getChange().equals(that.getChange())) return false;
        if (!this.getBuyers().equals(that.getBuyers())) return false;
        if (this.isBooleanField() != that.isBooleanField()) return false;
        if (this.getByteField() != that.getByteField()) return false;
        if (this.getCharField() != that.getCharField()) return false;
        if (this.getShortField() != that.getShortField()) return false;
        if (this.getIntField() != that.getIntField()) return false;
        if (this.getLongField() != that.getLongField()) return false;
        if (this.getFloatField() != that.getFloatField()) return false;
        if (this.getDoubleField() != that.getDoubleField()) return false;
        if (!this.getStringField().equals(that.getStringField())) return false;
        if (!this.getListField().equals(that.getListField())) return false;
        if (!this.getOptionalField().equals(that.getOptionalField())) return false;
        if (!this.getObjectField().equals(that.getObjectField())) return false;
        if (!this.getValue().equals(that.getValue())) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 0;
        result = 31 * result + getUid().hashCode();
        result = 31 * result + Long.hashCode(this.getItemId());
        result = 31 * result + getValidSubProducts().hashCode();
        result = 31 * result + Long.hashCode(this.getPrice());
        result = 31 * result + (this.isShowView() ? 1 : 0);
        result = 31 * result + getDescription().hashCode();
        result = 31 * result + getChange().hashCode();
        result = 31 * result + getBuyers().hashCode();
        result = 31 * result + (this.isBooleanField() ? 1 : 0);
        result = 31 * result + getByteField();
        result = 31 * result + getCharField();
        result = 31 * result + getShortField();
        result = 31 * result + getIntField();
        result = 31 * result + Long.hashCode(this.getLongField());
        result = 31 * result + Float.hashCode(this.getFloatField());
        result = 31 * result + Double.hashCode(this.getDoubleField());
        result = 31 * result + getStringField().hashCode();
        result = 31 * result + getListField().hashCode();
        result = 31 * result + getOptionalField().hashCode();
        result = 31 * result + getObjectField().hashCode();
        result = 31 * result + getValue().hashCode();
        return result;
    }
}