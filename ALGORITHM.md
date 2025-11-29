# Как работает Tailwind Merge

## Основная идея

Tailwind Merge решает проблему **конфликтующих классов Tailwind CSS**. Например, если у вас есть:

```javascript
twMerge("px-4 px-8")  // → "px-8"
twMerge("text-red-500 text-blue-500")  // → "text-blue-500"
```

Библиотека понимает, что `px-4` и `px-8` — это классы из **одной группы** (padding по оси X), поэтому последний класс "побеждает".

---

## Как работает группировка классов

### 1. Структура данных: Class Map (дерево классов)

В файле `class-group-utils.ts` создаётся **префиксное дерево (trie)** для быстрого поиска группы класса:

```typescript
export interface ClassPartObject {
    nextPart: Map<string, ClassPartObject>
    validators: ClassValidatorObject[] | null
    classGroupId: AnyClassGroupIds | undefined
}
```

Каждый узел дерева содержит:
- `nextPart` — Map для перехода к следующей части имени класса (разделённого `-`)
- `validators` — функции для проверки динамических значений
- `classGroupId` — ID группы, если на этом узле заканчивается класс

### 2. Определение групп в конфигурации

В `default-config.ts` определяются группы классов. Например:

```typescript
classGroups: {
    p: [{ p: scaleUnambiguousSpacing() }],
    px: [{ px: scaleUnambiguousSpacing() }],
    py: [{ py: scaleUnambiguousSpacing() }],
    // ... остальные группы padding
}
```

Формат определения группы:
- **Строка** (`'flex'`) — точное совпадение с именем класса
- **Объект** (`{ px: [...] }`) — префикс `px-`, за которым идут значения из массива
- **Функция-валидатор** (`isNumber`, `isArbitraryValue`) — проверяет динамические значения

### 3. Алгоритм определения группы класса

```typescript
const getClassGroupId = (className: string) => {
    if (className.startsWith('[') && className.endsWith(']')) {
        return getGroupIdForArbitraryProperty(className)
    }

    const classParts = className.split(CLASS_PART_SEPARATOR)
    const startIndex = classParts[0] === '' && classParts.length > 1 ? 1 : 0
    return getGroupRecursive(classParts, startIndex, classMap)
}
```

Алгоритм:
1. Разбивает имя класса по `-` (например, `text-red-500` → `['text', 'red', '500']`)
2. Рекурсивно спускается по дереву `classMap`
3. Если точного совпадения нет — пробует **валидаторы** на оставшейся части

### 4. Валидаторы для динамических значений

Валидаторы в `validators.ts` проверяют, является ли часть класса допустимым значением:

```typescript
export const isFraction = (value: string) => fractionRegex.test(value)
export const isNumber = (value: string) => !!value && !Number.isNaN(Number(value))
export const isInteger = (value: string) => !!value && Number.isInteger(Number(value))
export const isPercent = (value: string) => value.endsWith('%') && isNumber(value.slice(0, -1))
export const isTshirtSize = (value: string) => tshirtUnitRegex.test(value)
```

Например, для класса `px-4`:
- Движемся по дереву: `px` → узел найден
- На узле есть валидатор `isNumber`
- Проверяем `isNumber("4")` → `true`
- Возвращаем ID группы `px`

---

## Алгоритм слияния (mergeClassList)

```typescript
export const mergeClassList = (classList: string, configUtils: ConfigUtils) => {
    const { parseClassName, getClassGroupId, getConflictingClassGroupIds, sortModifiers } =
        configUtils

    /**
     * Set of classGroupIds in following format:
     * `{importantModifier}{variantModifiers}{classGroupId}`
     * @example 'float'
     * @example 'hover:focus:bg-color'
     * @example 'md:!pr'
     */
    const classGroupsInConflict: string[] = []
    // ... итерация справа налево
}
```

Ключевые шаги:
1. **Парсинг** — разбивает класс на модификаторы и базовый класс (`hover:px-4!` → modifiers: `['hover']`, base: `px-4`, important: `true`)
2. **Итерация справа налево** — последний класс имеет приоритет
3. **Определение группы** — вызывает `getClassGroupId`
4. **Проверка конфликтов** — если группа уже в `classGroupsInConflict`, класс пропускается
5. **Добавление конфликтующих групп** — использует `conflictingClassGroups`

---

## Конфликтующие группы

Некоторые группы **взаимоисключающие** по дизайну:

```typescript
conflictingClassGroups: {
    overflow: ['overflow-x', 'overflow-y'],
    overscroll: ['overscroll-x', 'overscroll-y'],
    inset: ['inset-x', 'inset-y', 'start', 'end', 'top', 'right', 'bottom', 'left'],
    'inset-x': ['right', 'left'],
    'inset-y': ['top', 'bottom'],
    flex: ['basis', 'grow', 'shrink'],
    gap: ['gap-x', 'gap-y'],
    p: ['px', 'py', 'ps', 'pe', 'pt', 'pr', 'pb', 'pl'],
    px: ['pr', 'pl'],
    py: ['pt', 'pb'],
    m: ['mx', 'my', 'ms', 'me', 'mt', 'mr', 'mb', 'ml'],
    mx: ['mr', 'ml'],
    my: ['mt', 'mb'],
    size: ['w', 'h'],
}
```

Это означает, что `p-4` вытесняет все более специфичные padding-классы (`px-2`, `pt-1`, и т.д.).

---

## Пример работы

```javascript
twMerge("p-4 px-2 hover:px-8 pt-1")
```

Итерация справа налево:
1. `pt-1` → группа `pt` → добавлено
2. `hover:px-8` → группа `hover:px` → добавлено
3. `px-2` → группа `px` → добавлено, но `pt` НЕ конфликтует
4. `p-4` → группа `p` → конфликтует с `px`, `pt` → **удалено**

Результат: `"px-2 hover:px-8 pt-1"`

---

## Резюме архитектуры

| Компонент | Роль |
|-----------|------|
| `default-config.ts` | Определение всех групп классов и их конфликтов |
| `class-group-utils.ts` | Построение дерева для быстрого поиска группы |
| `validators.ts` | Проверка динамических значений (числа, дроби, размеры) |
| `parse-class-name.ts` | Разбор модификаторов (`hover:`, `md:`, `!`) |
| `merge-classlist.ts` | Основной алгоритм слияния |

---

## Дополнительные особенности

### Модификаторы

Класс `hover:px-4` и `px-4` — это **разные** классы с точки зрения конфликтов. Модификаторы (`hover:`, `md:`, `focus:` и т.д.) учитываются при формировании ID конфликта:

```typescript
const classId = modifierId + classGroupId
// "hover:px" и "px" — разные ID
```

### Important модификатор

Модификатор `!` (important) также учитывается отдельно:

```typescript
const modifierId = hasImportantModifier
    ? variantModifier + IMPORTANT_MODIFIER
    : variantModifier
```

### Postfix модификаторы

Классы вида `bg-red-500/50` содержат postfix модификатор (`/50` — opacity). Библиотека сначала пытается найти группу без postfix, затем с ним.

### LRU Cache

Для оптимизации используется LRU кэш результатов слияния:

```typescript
cacheSize: 500  // в default-config.ts
```
