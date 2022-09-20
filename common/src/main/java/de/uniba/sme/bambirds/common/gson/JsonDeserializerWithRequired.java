package de.uniba.sme.bambirds.common.gson;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSyntaxException;
import com.google.gson.annotations.SerializedName;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

/**
 * Adds the feature to use required fields in models.
 *
 * @param <T> Model to parse to.
 */
public class JsonDeserializerWithRequired<T> implements JsonDeserializer<T> {

	private final Gson gson = new Gson();

	/**
	 * Called when the model is being parsed.
	 *
	 * @param je   Source json string.
	 * @param type Object's model.
	 * @param jdc  Unused in this case.
	 * @return Parsed object.
	 * @throws JsonParseException When parsing is impossible.
	 */
	@Override
	public T deserialize(final JsonElement je, final Type type, final JsonDeserializationContext jdc)
			throws JsonParseException {


		try {
			// Getting all fields of the class and checking if all required ones were provided.
			Class<?> typeClass;
			if (type instanceof Class) {
				typeClass = (Class<?>) type;
			} else {
				typeClass = getClass().getClassLoader().loadClass(type.getTypeName());
			}
			checkRequiredFields(je, typeClass);
			// Checking if all required fields of parent classes were provided.
			checkSuperClasses(je, typeClass);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}

		// All checks are ok and parsing as usual.
		return gson.fromJson(je, type);
	}

	/**
	 * Checks whether all required fields were provided in the class.
	 *
	 * @param je             Fields to be checked.
	 * @param specifiedClass Instance to check fields in.
	 * @throws JsonParseException When some required field was not met.
	 */
	private void checkRequiredFields(final JsonElement je, final Class<?> specifiedClass)
			throws JsonParseException {
		// Checking nested list items too.
		if (je.isJsonArray()) {
			final JsonArray jsonArray = je.getAsJsonArray();
			for (final JsonElement jsonArrayElement : jsonArray) {
				checkRequiredFields(jsonArrayElement, specifiedClass);
				checkSuperClasses(jsonArrayElement, specifiedClass);
			}
		}

		if (je.isJsonObject()) {
			JsonObject jo = je.getAsJsonObject();
			for (Field f : specifiedClass.getDeclaredFields()) {
				// If some field has required annotation.
				if (f.getAnnotation(JsonRequired.class) != null) {
					try {
						String fieldName = f.getName();
						if ((f.getAnnotation(SerializedName.class)) != null) {
							fieldName = f.getAnnotation(SerializedName.class).value();
						}
						if (jo.has(fieldName) && !jo.get(fieldName).isJsonNull()) {
							checkRequiredFields(jo.get(fieldName), f.getType());
							if (!(f.getType().isPrimitive() || f.getType().isEnum()) || !jo.get(fieldName).isJsonPrimitive()) {
								checkSuperClasses(jo.get(fieldName), f.getType());
							}
						} else {
							throw new JsonSyntaxException(String.format("%1$s -> %2$s is required",
									specifiedClass.getSimpleName(),
									f.getName()));
						}
					} catch (IllegalArgumentException e) {
						// Exceptions while reflection.
						throw new JsonParseException(e);
					}
				}
			}
		} else if (!
				(je.isJsonPrimitive()
						&& (specifiedClass.isPrimitive()
						|| (je.getAsJsonPrimitive().isString()
						&& (specifiedClass.isEnum()
						|| specifiedClass == String.class)
				)))
		) {
			throw new JsonSyntaxException(String.format("Json Element %1$s does not correspond with Type %2$s",
					je,
					specifiedClass.getTypeName()));
		}


	}

	/**
	 * Checks whether all super classes have all required fields.
	 *
	 * @param jsonElement    Object to check required fields in its superclasses.
	 * @param specifiedClass Class of the Element
	 * @throws JsonParseException When some required field was not met.
	 */
	private void checkSuperClasses(final JsonElement jsonElement, final Class<?> specifiedClass) throws JsonParseException {

		Class<?> superclass = specifiedClass;
		while ((superclass = superclass.getSuperclass()) != null && superclass != Object.class) {
			checkRequiredFields(jsonElement, superclass);
		}
	}

}
