package constant

import "fmt"

func Make(v any) Value {
	switch underlyingV := v.(type) {
	case int:
		return MakeInt(underlyingV)
	case bool:
		return MakeBool(underlyingV)
	case string:
		return MakeString(underlyingV)
	default:
		panic(fmt.Sprintf("unsupported type %T", v))
	}
}

func MakeInt(v int) Value {
	return intValue{
		literal: fmt.Sprintf("%d", v),
		v:       int64(v),
	}
}

func MakeString(v string) Value {
	return stringValue{
		literal: v,
		v:       v,
	}
}

func MakeBool(v bool) Value {
	return boolValue{
		literal: fmt.Sprintf("%v", v),
		v:       v,
	}
}
