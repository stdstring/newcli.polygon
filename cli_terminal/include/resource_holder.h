#ifndef H_RESOURCE_HOLDER
#define H_RESOURCE_HOLDER

#include <functional>

template <typename R> class ResourceHolder
{
public:
    ResourceHolder() = delete;
    ResourceHolder(const ResourceHolder&) = delete;
    ResourceHolder(ResourceHolder&&) = delete;
    ResourceHolder& operator=(const ResourceHolder&) = delete;
    ResourceHolder& operator=(ResourceHolder&&) = delete;

    ResourceHolder(R resource, std::function<void(R)> resource_cleaner) : _resource(resource), _resource_cleaner(resource_cleaner) {}
    ~ResourceHolder() { _resource_cleaner(_resource); }

    R get() const { return _resource; }
private:
    R _resource;
    std::function<void(R)> _resource_cleaner;
};

#endif